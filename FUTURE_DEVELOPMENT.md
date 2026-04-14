# maceR Future Development Roadmap

**Created**: April 15, 2026  
**Document Purpose**: Track architectural improvements and refactoring opportunities  
**Target Audience**: Development team planning future iterations

---

## Overview

This document outlines 10 major architectural improvements recommended for the maceR codebase. Each improvement is documented with current state, proposed solution, code examples, and implementation guidance.

**Quick Links**:
- [High Priority Items](#high-priority-high-impact-low-effort)
- [Medium Priority Items](#medium-priority-medium-impact-medium-effort)
- [Lower Priority Items](#lower-priority-high-impact-higher-effort)

---

## 1. Consolidate Environment Management

### Current State
- Four separate environment pointers: `env1`, `env2`, `env3`, `env4`
- Stored in `App_settings` object
- Opaque access pattern: `App_settings$env4$Annotate$Actograms`
- Difficult to track ownership and dependencies

### Problems
- Hard to understand data flow
- No type safety or validation
- Difficult to refactor safely
- Confusing for new developers

### Proposed Solution
Create a unified `DataStore` R6 class that encapsulates all data objects.

### Implementation Code
```r
# R/fct_R6_DataStore.R

DataStore <- R6::R6Class("DataStore",
  public = list(
    raw_mice = NULL,
    clean_mice = NULL,
    tables = NULL,
    annotate = NULL,
    
    initialize = function() {
      self$raw_mice <- list()
      self$clean_mice <- list()
      self$tables <- Custom_tables$new()
      self$annotate <- Annotate$new()
    },
    
    #' Get all raw data
    get_raw_data = function() {
      self$raw_mice
    },
    
    #' Get cleaned animals
    get_clean_mice = function() {
      self$clean_mice
    },
    
    #' Check if data is loaded
    is_initialized = function() {
      length(self$raw_mice) > 0 && length(self$clean_mice) > 0
    }
  )
)
```

### Usage Changes
```r
# Old:
env$env4$Annotate$Actograms

# New:
App_settings$data_store$annotate$Actograms
```

### Migration Path
1. Create `DataStore` class
2. Update `load_data()` to populate `data_store` instead of `env1-4`
3. Update all modules to use `App_settings$data_store` instead of `App_settings$env*`
4. Remove old `env1-4` fields after testing
5. Update developer guide

### Files to Modify
- `R/fct_R6_DataStore.R` (new)
- `R/fct_data_loader.R`
- `R/app_server.R`
- `R/mod_*.R` (all modules)
- Documentation

### Benefits
- Self-documenting code
- Type-safe access patterns
- Easier testing and refactoring
- Clear dependency graph

### Effort Estimate
**Time**: 2-3 hours  
**Complexity**: Medium  
**Risk**: Low (non-breaking if done carefully)

---

## 2. Add Data Validation Layer

### Current State
- Basic file format checking in `mod_Input_DF`
- No schema validation
- Errors occur downstream during analysis
- Poor error messages for users

### Problems
- Invalid data reaches analysis functions
- Hard to diagnose issues
- Prevents batch processing
- User frustration

### Proposed Solution
Create comprehensive validation framework with schema definitions.

### Implementation Code
```r
# R/utils_validation.R

#' Define data schema
create_schema <- function() {
  list(
    activity_file = list(
      required_columns = c("timepoint", "activity"),
      column_types = c(timepoint = "numeric", activity = "numeric"),
      constraints = list(
        timepoint_ascending = function(x) all(diff(x) >= 0),
        activity_non_negative = function(x) all(x >= 0)
      )
    ),
    metadata_file = list(
      required_columns = c("id", "Sex", "Genotype", "Cabinet"),
      column_types = c(
        id = "character",
        Sex = c("M", "F"),
        Genotype = "character",
        Cabinet = "integer"
      )
    )
  )
}

#' Validate data against schema
validate_data <- function(df, schema_type, schema) {
  errors <- list()
  
  # Check required columns
  missing_cols <- setdiff(schema[[schema_type]]$required_columns, names(df))
  if (length(missing_cols) > 0) {
    errors$missing_columns <- missing_cols
  }
  
  # Check column types
  for (col in names(schema[[schema_type]]$column_types)) {
    expected_type <- schema[[schema_type]]$column_types[[col]]
    if (is.character(expected_type)) {
      if (expected_type == "numeric" && !is.numeric(df[[col]])) {
        errors$type_mismatch <- c(errors$type_mismatch, col)
      }
    } else if (is.character(expected_type)) {
      # Factor: check valid values
      invalid_vals <- setdiff(df[[col]], expected_type)
      if (length(invalid_vals) > 0) {
        errors[[paste0(col, "_invalid")]] <- invalid_vals
      }
    }
  }
  
  # Check constraints
  constraints <- schema[[schema_type]]$constraints
  for (constraint_name in names(constraints)) {
    constraint_fn <- constraints[[constraint_name]]
    if (!constraint_fn(df[[names(schema[[schema_type]]$column_types)[1]]])) {
      errors[[constraint_name]] <- TRUE
    }
  }
  
  if (length(errors) > 0) {
    stop("Validation failed:\n", 
         paste(names(errors), collapse = "\n"))
  }
  
  TRUE
}

#' Create user-friendly error message
format_validation_error <- function(errors) {
  messages <- list()
  if (!is.null(errors$missing_columns)) {
    messages$missing <- sprintf(
      "Missing required columns: %s",
      paste(errors$missing_columns, collapse = ", ")
    )
  }
  if (!is.null(errors$type_mismatch)) {
    messages$type <- sprintf(
      "Invalid data types in columns: %s",
      paste(errors$type_mismatch, collapse = ", ")
    )
  }
  
  paste(unlist(messages), collapse = "\n")
}
```

### Usage in mod_Input_DF
```r
observeEvent(input$upload_files, {
  schema <- create_schema()
  
  tryCatch({
    validate_data(activity_data, "activity_file", schema)
    validate_data(metadata, "metadata_file", schema)
    # Proceed with upload
  }, error = function(e) {
    showModal(modalDialog(
      title = "Data Validation Error",
      format_validation_error(e$message),
      easyClose = TRUE
    ))
  })
})
```

### Files to Modify
- `R/utils_validation.R` (new)
- `R/mod_Input_DF.R`
- `R/fct_data_loader.R`

### Benefits
- Fail fast with clear errors
- Better user experience
- Enables automated testing
- Prevents downstream crashes
- Supports batch validation

### Effort Estimate
**Time**: 3-4 hours  
**Complexity**: Medium  
**Risk**: Low

---

## 3. Implement Caching/Memoization

### Current State
- All computations run fresh each time
- Periodograms recalculated on every analysis run
- No mechanism to cache expensive operations
- Slow interaction when user refines parameters

### Problems
- Poor performance with large datasets
- Unnecessary CPU usage
- Laggy UI response
- Users may think app froze

### Proposed Solution
Add memoization layer for analysis computations.

### Implementation Code
```r
# R/fct_cache.R

CacheManager <- R6::R6Class("CacheManager",
  public = list(
    cache = list(),
    max_size = 1000,  # MB
    current_size = 0,
    
    initialize = function(max_size_mb = 1000) {
      self$max_size <- max_size_mb * 1024 * 1024  # Convert to bytes
    },
    
    #' Generate cache key from parameters
    make_key = function(data_id, analysis_type, params) {
      params_str <- paste(names(params), params, 
                         sep = "=", collapse = "|")
      key <- digest::digest(paste(data_id, analysis_type, 
                                  params_str, sep = ":"))
      key
    },
    
    #' Get cached result
    get = function(key) {
      if (key %in% names(self$cache)) {
        self$cache[[key]]$result
      } else {
        NULL
      }
    },
    
    #' Store result in cache
    store = function(key, result, metadata = NULL) {
      size <- object.size(result)
      
      # Check size limit
      if (self$current_size + size > self$max_size) {
        self$evict_oldest()
      }
      
      self$cache[[key]] <- list(
        result = result,
        timestamp = Sys.time(),
        metadata = metadata,
        size = size
      )
      self$current_size <- self$current_size + size
    },
    
    #' Remove oldest entry
    evict_oldest = function() {
      if (length(self$cache) == 0) return()
      
      timestamps <- sapply(self$cache, function(x) x$timestamp)
      oldest_key <- names(timestamps)[which.min(timestamps)]
      
      self$current_size <- self$current_size - self$cache[[oldest_key]]$size
      self$cache[[oldest_key]] <- NULL
    },
    
    #' Clear all cache
    clear = function() {
      self$cache <- list()
      self$current_size <- 0
    }
  )
)

# Wrapper function for cached analysis
cached_analysis <- function(data, analysis_type, params, 
                           cache_mgr, analysis_fn) {
  key <- cache_mgr$make_key(
    digest::digest(data), 
    analysis_type, 
    params
  )
  
  # Try to get from cache
  cached_result <- cache_mgr$get(key)
  if (!is.null(cached_result)) {
    message("Using cached result")
    return(cached_result)
  }
  
  # Compute if not cached
  message("Computing (this may take a moment)...")
  result <- analysis_fn(data, analysis_type, params)
  
  # Store in cache
  cache_mgr$store(key, result, list(
    data_id = digest::digest(data),
    analysis_type = analysis_type
  ))
  
  result
}
```

### Integration with Annotate
```r
# Add cache to App_settings initialization
App_settings$cache <- CacheManager$new(max_size_mb = 500)

# In analysis modules:
Annotate$plot_periodogram <- function(env, params) {
  result <- cached_analysis(
    data = env$env3$Custom_tables$periodograms[[1]],
    analysis_type = "periodogram",
    params = params,
    cache_mgr = App_settings$cache,
    analysis_fn = function(data, type, p) {
      # Original plotting code
      zeitgebr::periodogram(data)
    }
  )
  self$period_plots$Per1 <- result
}
```

### Files to Modify
- `R/fct_cache.R` (new)
- `R/fct_R6_App_settings.R` (add cache field)
- `R/fct_R6_Annotate.R` (wrap compute functions)
- `DESCRIPTION` (add digest dependency)

### Benefits
- Faster UI response (100-1000x for repeated operations)
- Reduced CPU usage
- Better user experience
- Automatic memory management

### Effort Estimate
**Time**: 4-5 hours  
**Complexity**: Medium  
**Risk**: Low (errors don't break app, just cause recomputation)

---

## 4. Add Centralized Logging/Error Handling

### Current State
- Errors scattered throughout the app
- No audit trail or logging
- Difficult to debug production issues
- No usage analytics

### Problems
- Hard to troubleshoot user issues
- No record of what went wrong
- Silent failures possible
- No performance metrics

### Proposed Solution
Implement centralized logging system.

### Implementation Code
```r
# R/fct_logger.R

Logger <- R6::R6Class("Logger",
  public = list(
    log_file = NULL,
    level = "INFO",
    
    initialize = function(log_path = NULL, level = "INFO") {
      if (is.null(log_path)) {
        log_path <- file.path(tempdir(), "maceR.log")
      }
      self$log_file <- log_path
      self$level <- level
      
      # Create header
      self$write(sprintf(
        "=== maceR Session Start: %s ===\n",
        format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      ))
    },
    
    write = function(message) {
      cat(message, "\n", file = self$log_file, append = TRUE)
    },
    
    debug = function(message) {
      if (self$level %in% c("DEBUG")) {
        self$write(sprintf("[DEBUG] %s", message))
      }
    },
    
    info = function(message) {
      if (self$level %in% c("DEBUG", "INFO")) {
        self$write(sprintf("[INFO] %s", message))
      }
    },
    
    warn = function(message) {
      self$write(sprintf("[WARN] %s", message))
      warning(message)
    },
    
    error = function(message, error_obj = NULL) {
      timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      self$write(sprintf("[ERROR] %s: %s", timestamp, message))
      
      if (!is.null(error_obj)) {
        self$write(sprintf("  Traceback: %s", 
                          paste(error_obj$traceback, collapse = "\n  ")))
      }
    },
    
    get_logs = function(n_lines = 50) {
      if (!file.exists(self$log_file)) return(character(0))
      
      all_lines <- readLines(self$log_file)
      tail(all_lines, n_lines)
    }
  )
)

# Global logger instance
.maceR_logger <- NULL

init_logger <- function(log_path = NULL, level = "INFO") {
  .maceR_logger <<- Logger$new(log_path, level)
}

get_logger <- function() {
  if (is.null(.maceR_logger)) {
    init_logger()
  }
  .maceR_logger
}
```

### Usage Throughout App
```r
# In run_app.R
run_app <- function(...) {
  init_logger()
  logger <- get_logger()
  logger$info("Starting maceR application")
  # ... rest of startup
}

# In mod_Input_DF
observeEvent(input$upload_files, {
  logger <- get_logger()
  logger$info(sprintf("User uploaded %d files", length(input$files)))
  
  tryCatch({
    # processing
    logger$info("Files validated successfully")
  }, error = function(e) {
    logger$error("File validation failed", e)
  })
})

# In mod_analysis
observeEvent(input$run_analysis, {
  logger <- get_logger()
  start_time <- Sys.time()
  
  Annotate$plot_periodogram(...)
  
  elapsed <- difftime(Sys.time(), start_time, units = "secs")
  logger$info(sprintf("Analysis completed in %.2f seconds", elapsed))
})
```

### Files to Modify
- `R/fct_logger.R` (new)
- `R/run_app.R`
- `R/mod_*.R` (all modules, add key logging points)
- `R/app_server.R` (central error handling)

### Benefits
- Debugging and troubleshooting
- Usage analytics
- Performance monitoring
- Audit trail for compliance
- Early warning system

### Effort Estimate
**Time**: 2-3 hours  
**Complexity**: Low  
**Risk**: Very Low

---

## 5. Separate UI State from Business State

### Current State
- `App_settings$plotTab` mixes UI visibility with business logic
- No clear separation between what's visible and what's available
- Difficult to manage UI independently from data

### Problems
- Confusing mental model
- Hard to test
- Difficult to maintain UI state independently
- Brittle dependencies

### Proposed Solution
Create separate state objects for UI and business logic.

### Implementation Code
```r
# R/fct_R6_UIState.R

UIState <- R6::R6Class("UIState",
  public = list(
    visible_tabs = list(),
    active_tab = NULL,
    sidebar_expanded = TRUE,
    show_debug_panel = FALSE,
    plot_layout = "grid",  # or "list"
    
    initialize = function() {
      self$visible_tabs <- list(
        home = TRUE,
        input = FALSE,
        data_structure = FALSE,
        your_data = FALSE,
        analysis = FALSE,
        plots = FALSE,
        reports = FALSE
      )
    },
    
    show_tab = function(tab_name) {
      self$visible_tabs[[tab_name]] <- TRUE
      self$active_tab <- tab_name
    },
    
    hide_tab = function(tab_name) {
      self$visible_tabs[[tab_name]] <- FALSE
    },
    
    tab_visible = function(tab_name) {
      isTRUE(self$visible_tabs[[tab_name]])
    },
    
    all_visible_tabs = function() {
      names(self$visible_tabs)[unlist(self$visible_tabs)]
    }
  )
)

# R/fct_R6_BusinessState.R

BusinessState <- R6::R6Class("BusinessState",
  public = list(
    data_loaded = FALSE,
    data_validated = FALSE,
    analysis_complete = FALSE,
    report_available = FALSE,
    current_animals = 0,
    
    mark_data_loaded = function() {
      self$data_loaded <- TRUE
    },
    
    mark_data_validated = function() {
      self$data_validated <- TRUE
    },
    
    mark_analysis_complete = function() {
      self$analysis_complete <- TRUE
      self$report_available <- TRUE
    },
    
    reset_analysis = function() {
      self$analysis_complete <- FALSE
      self$report_available <- FALSE
    },
    
    can_proceed = function(to_step) {
      switch(to_step,
        "analysis" = self$data_loaded && self$data_validated,
        "report" = self$analysis_complete,
        FALSE
      )
    }
  )
)
```

### Usage in App
```r
# Initialization
App_settings$ui_state <- UIState$new()
App_settings$business_state <- BusinessState$new()

# In module
observeEvent(dataStructure_out$YourDataTab(), {
  if (isTRUE(dataStructure_out$YourDataTab())) {
    App_settings$business_state$mark_data_validated()
    App_settings$ui_state$show_tab("your_data")
  }
})
```

### Files to Modify
- `R/fct_R6_UIState.R` (new)
- `R/fct_R6_BusinessState.R` (new)
- `R/fct_R6_App_settings.R`
- `R/mod_*.R` (update to use new state objects)

### Benefits
- Clear separation of concerns
- Easier to test
- More maintainable
- Independent UI refresh
- Better debugging

### Effort Estimate
**Time**: 3-4 hours  
**Complexity**: Medium  
**Risk**: Medium (behavioral changes)

---

## 6. Use Factory Pattern for Object Creation

### Current State
```r
# In load_data():
Annotate <- Annotate$new()
Custom_tables <- Custom_tables$new()
# Scattered throughout
```

- Objects created inline
- Difficult to swap implementations
- Hard dependency injection

### Proposed Solution
Create factory for consistent object creation.

### Implementation Code
```r
# R/fct_object_factory.R

ObjectFactory <- R6::R6Class("ObjectFactory",
  public = list(
    # Configuration
    config = list(),
    
    initialize = function(config = list()) {
      self$config <- config
    },
    
    create_raw_mouse_data = function(id) {
      obj <- Raw_mouse_data$new()
      obj$id <- id
      obj
    },
    
    create_clean_mouse_data = function() {
      Clean_mouse_data$new()
    },
    
    create_custom_tables = function() {
      Custom_tables$new()
    },
    
    create_annotate = function() {
      Annotate$new()
    },
    
    create_data_store = function() {
      DataStore$new()
    },
    
    create_logger = function() {
      Logger$new(
        log_path = self$config$log_path %||% NULL,
        level = self$config$log_level %||% "INFO"
      )
    }
  )
)

# Usage
factory <- ObjectFactory$new(list(
  log_path = "logs/maceR.log",
  log_level = "DEBUG"
))

annotate <- factory$create_annotate()
logger <- factory$create_logger()
```

### Files to Modify
- `R/fct_object_factory.R` (new)
- `R/fct_data_loader.R`
- `R/app_server.R`

### Benefits
- Consistent object creation
- Easy to mock for testing
- Dependency injection ready
- Configuration-driven initialization

### Effort Estimate
**Time**: 1-2 hours  
**Complexity**: Low  
**Risk**: Low

---

## 7. Add Progress Tracking

### Current State
- Long operations complete silently
- User unsure if app is responsive
- No feedback during heavy computation

### Problems
- Poor user experience
- Users think app froze
- No indication of time remaining

### Proposed Solution
Add progress callbacks to all long operations.

### Implementation Code
```r
# R/fct_progress.R

#' Create progress tracker
progress_tracker <- function(total_steps, title = "Processing") {
  list(
    total = total_steps,
    current = 0,
    title = title,
    start_time = Sys.time(),
    
    update = function(step, message = NULL) {
      self$current <- step
      percent <- (step / self$total) * 100
      elapsed <- difftime(Sys.time(), self$start_time, units = "secs")
      
      if (!is.null(message)) {
        cat(sprintf("%s: %d/%d (%.1f%%) - %s [%.1fs]\n",
                   self$title, step, self$total, percent, 
                   message, as.numeric(elapsed)))
      }
    }
  )
}

# Enhanced load_data with progress
load_data_with_progress <- function(env, progress_callback = NULL) {
  total_steps <- 5
  
  if (!is.null(progress_callback)) {
    progress_callback(1/total_steps, "Loading raw data...")
  }
  myMice2 <- env$env1$myMice
  myCleanMice <- list()
  
  if (!is.null(progress_callback)) {
    progress_callback(2/total_steps, "Cleaning data...")
  }
  for (i in seq_along(myMice2)) {
    myCleanMice[[i]] <- Clean_mouse_data$new()
    myCleanMice[[i]]$compile(myMice2[[i]])
  }
  
  if (!is.null(progress_callback)) {
    progress_callback(3/total_steps, "Creating tables...")
  }
  Custom_tables <- Custom_tables$new()
  Custom_tables$compile(env$env2)
  
  if (!is.null(progress_callback)) {
    progress_callback(4/total_steps, "Finalizing...")
  }
  Annotate <- Annotate$new()
  
  if (!is.null(progress_callback)) {
    progress_callback(5/total_steps, "Complete")
  }
}
```

### Usage in Shiny
```r
# In module
observeEvent(input$analyze, {
  withProgress(message = "Running analysis...", value = 0, {
    load_data_with_progress(env, function(progress, message) {
      incProgress(progress, detail = message)
    })
  })
})
```

### Files to Modify
- `R/fct_progress.R` (new)
- `R/fct_data_loader.R`
- `R/mod_analysis.R`
- `R/mod_report.R`

### Benefits
- Better UX
- Users know app is working
- Transparency
- Can stop long operations

### Effort Estimate
**Time**: 2-3 hours  
**Complexity**: Low  
**Risk**: Very Low

---

## 8. Module Registration System

### Current State
```r
# In app_server.R - manual calls
mod_Input_DF_server("Input_DF_ui_1", App_settings)
mod_data_structure_server("data_structure_ui_1", App_settings)
# ... many more
```

- Manual module registration
- Implicit dependencies
- Hard to manage as app grows

### Proposed Solution
Create module registry for declarative management.

### Implementation Code
```r
# R/fct_module_registry.R

ModuleRegistry <- R6::R6Class("ModuleRegistry",
  public = list(
    modules = list(),
    
    register = function(name, ui_fn, server_fn, 
                       dependencies = NULL, config = NULL) {
      self$modules[[name]] <- list(
        ui = ui_fn,
        server = server_fn,
        deps = dependencies,
        config = config
      )
      invisible(self)
    },
    
    get_module = function(name) {
      if (!name %in% names(self$modules)) {
        stop(sprintf("Module '%s' not registered", name))
      }
      self$modules[[name]]
    },
    
    get_dependencies = function(name) {
      self$get_module(name)$deps
    },
    
    is_registered = function(name) {
      name %in% names(self$modules)
    },
    
    list_modules = function() {
      names(self$modules)
    }
  )
)

# Usage
module_registry <- ModuleRegistry$new()

module_registry$register(
  "input_df",
  ui_fn = mod_Input_DF_ui,
  server_fn = mod_Input_DF_server,
  dependencies = NULL
)

module_registry$register(
  "data_structure",
  ui_fn = mod_data_structure_ui,
  server_fn = mod_data_structure_server,
  dependencies = c("input_df")
)

module_registry$register(
  "analysis",
  ui_fn = mod_analysis_ui,
  server_fn = mod_analysis_server,
  dependencies = c("data_structure")
)

# Initialization
init_modules <- function(registry, App_settings) {
  for (module_name in registry$list_modules()) {
    module <- registry$get_module(module_name)
    module$server(module_name, App_settings)
  }
}
```

### Files to Modify
- `R/fct_module_registry.R` (new)
- `R/app_server.R`
- `R/app_ui.R`

### Benefits
- Automatic dependency resolution
- Easy to add/remove modules
- Self-documenting module graph
- Can validate dependencies at startup

### Effort Estimate
**Time**: 2-3 hours  
**Complexity**: Medium  
**Risk**: Medium

---

## 9. Configuration Management

### Current State
- Hard-coded settings throughout:
  - Pandoc path: `/opt/homebrew/bin/pandoc`
  - Temp directories: `tempdir()`
  - Cache sizes: inline numbers
  - Plot dimensions: scattered

### Problems
- Difficult deployment
- Environment-specific issues
- No centralized settings
- Hard to customize

### Proposed Solution
Implement configuration system with file-based and environment variable support.

### Implementation Code
```r
# R/fct_config.R

#' Load configuration
load_config <- function(config_file = NULL) {
  default_config <- list(
    # Paths
    pandoc_path = Sys.getenv("PANDOC_PATH", "/opt/homebrew/bin/pandoc"),
    temp_dir = Sys.getenv("MACE_TEMP_DIR", tempdir()),
    log_dir = Sys.getenv("MACE_LOG_DIR", "logs"),
    
    # Limits
    max_animals = as.numeric(Sys.getenv("MACE_MAX_ANIMALS", 1000)),
    max_datapoints = as.numeric(Sys.getenv("MACE_MAX_DATAPOINTS", 1000000)),
    cache_size_mb = as.numeric(Sys.getenv("MACE_CACHE_SIZE", 500)),
    
    # Plotting
    plot_width = as.numeric(Sys.getenv("MACE_PLOT_WIDTH", 800)),
    plot_height = as.numeric(Sys.getenv("MACE_PLOT_HEIGHT", 600)),
    
    # Report
    report_template_dir = system.file(
      "rmarkdown/templates/maceR_report",
      package = "maceR"
    ),
    
    # Logging
    log_level = Sys.getenv("MACE_LOG_LEVEL", "INFO"),
    enable_debug = as.logical(Sys.getenv("MACE_DEBUG", "FALSE"))
  )
  
  # Load from file if provided
  if (!is.null(config_file) && file.exists(config_file)) {
    file_config <- yaml::read_yaml(config_file)
    default_config <- utils::modifyList(default_config, file_config)
  }
  
  default_config
}

# Usage
config <- load_config("~/.maceR/config.yaml")

# Access config
if (file.exists(config$pandoc_path)) {
  Sys.setenv(PATH = paste(dirname(config$pandoc_path), 
                        Sys.getenv("PATH"), sep = ":"))
}
```

### Configuration File Format
```yaml
# ~/.maceR/config.yaml
pandoc_path: /usr/local/bin/pandoc
temp_dir: /var/tmp/maceR
log_dir: ~/.maceR/logs
cache_size_mb: 1000

max_animals: 500
max_datapoints: 10000000

plot_width: 1024
plot_height: 768

log_level: DEBUG
enable_debug: false
```

### Files to Modify
- `R/fct_config.R` (new)
- `R/run_app.R`
- `DESCRIPTION` (add yaml dependency)
- Create `inst/examples/config.yaml`

### Benefits
- Environment-specific settings
- Easy deployment
- Configuration without code changes
- Better security (no hardcoded paths)

### Effort Estimate
**Time**: 1-2 hours  
**Complexity**: Low  
**Risk**: Low

---

## 10. Add Session Recovery

### Current State
- Browser refresh loses all work
- No session persistence
- Users must restart analysis

### Problems
- Poor resilience
- Lost productivity
- Network glitches cause data loss
- Bad user experience

### Proposed Solution
Implement session state persistence and recovery.

### Implementation Code
```r
# R/fct_session_manager.R

SessionManager <- R6::R6Class("SessionManager",
  public = list(
    session_dir = NULL,
    
    initialize = function(base_dir = "sessions") {
      self$session_dir <- base_dir
      dir.create(base_dir, showWarnings = FALSE)
    },
    
    save_session = function(session_id, App_settings) {
      session_file <- file.path(self$session_dir, 
                               paste0(session_id, ".rdata"))
      
      # Save only serializable objects
      session_data <- list(
        metadata = App_settings$metadata$name,
        data_list = App_settings$dataList$name,
        subsetting = App_settings$subsetting,
        ld_params = App_settings$LDparams,
        timestamp = Sys.time()
      )
      
      saveRDS(session_data, session_file)
    },
    
    load_session = function(session_id) {
      session_file <- file.path(self$session_dir, 
                               paste0(session_id, ".rdata"))
      
      if (file.exists(session_file)) {
        readRDS(session_file)
      } else {
        NULL
      }
    },
    
    session_exists = function(session_id) {
      session_file <- file.path(self$session_dir, 
                               paste0(session_id, ".rdata"))
      file.exists(session_file)
    },
    
    list_sessions = function() {
      session_files <- list.files(self$session_dir, 
                                 pattern = "*.rdata")
      sub(".rdata$", "", session_files)
    },
    
    delete_session = function(session_id) {
      session_file <- file.path(self$session_dir, 
                               paste0(session_id, ".rdata"))
      if (file.exists(session_file)) {
        file.remove(session_file)
        return(TRUE)
      }
      FALSE
    }
  )
)

# Integration with Shiny
mod_session_recovery_ui <- function(id) {
  ns <- NS(id)
  tagList(
    conditionalPanel(
      condition = "output.previous_session_exists",
      div(
        class = "alert alert-info",
        p("A previous session was found. Would you like to restore it?"),
        actionButton(ns("restore"), "Restore Session"),
        actionButton(ns("new_session"), "Start New Session")
      )
    )
  )
}

mod_session_recovery_server <- function(id, App_settings, 
                                       session_mgr) {
  moduleServer(id, function(input, output, session) {
    session_id <- session$token
    
    output$previous_session_exists <- reactive({
      session_mgr$session_exists(session_id)
    })
    outputOptions(output, "previous_session_exists", 
                 suspendWhenHidden = FALSE)
    
    observeEvent(input$restore, {
      session_data <- session_mgr$load_session(session_id)
      if (!is.null(session_data)) {
        # Restore App_settings
        App_settings$metadata <- session_data$metadata
        App_settings$dataList <- session_data$data_list
        App_settings$subsetting <- session_data$subsetting
        App_settings$LDparams <- session_data$ld_params
        
        showNotification("Session restored", type = "message")
      }
    })
    
    # Auto-save periodically
    observe({
      invalidateLater(60000)  # Every 60 seconds
      session_mgr$save_session(session_id, App_settings)
    })
  })
}
```

### Files to Modify
- `R/fct_session_manager.R` (new)
- `R/mod_session_recovery.R` (new)
- `R/app_server.R`
- `R/app_ui.R`

### Benefits
- Resilience to network issues
- Better user experience
- Work preservation
- Session history available

### Effort Estimate
**Time**: 3-4 hours  
**Complexity**: Medium  
**Risk**: Medium

---

# Implementation Priority Matrix

## High Priority - High Impact, Low Effort
1. **Consolidate Environment Management** (1)
2. **Add Data Validation Layer** (2)
3. **Configuration Management** (9)

**Recommended for next release**. These provide significant improvements with manageable effort.

## Medium Priority - Medium Impact, Medium Effort
4. **Add Centralized Logging/Error Handling** (4)
5. **Separate UI State from Business State** (5)
6. **Module Registration System** (8)
7. **Add Progress Tracking** (7)

**Recommended for release 2-3**. These improve developer experience and user experience significantly.

## Lower Priority - High Impact, Higher Effort
8. **Use Factory Pattern** (6)
9. **Implement Caching/Memoization** (3)
10. **Add Session Recovery** (10)

**Recommended for later releases**. These provide major improvements but require more development time.

---

# Implementation Timeline

### Phase 1 (Weeks 1-2)
- [ ] Implement Configuration Management (9)
- [ ] Add Data Validation Layer (2)
- [ ] Consolidate Environment Management (1)

### Phase 2 (Weeks 3-4)
- [ ] Add Centralized Logging (4)
- [ ] Add Progress Tracking (7)
- [ ] Use Factory Pattern (6)

### Phase 3 (Weeks 5-6)
- [ ] Separate UI/Business State (5)
- [ ] Module Registration (8)
- [ ] Implement Caching (3)

### Phase 4 (Weeks 7+)
- [ ] Add Session Recovery (10)
- [ ] Additional features/bug fixes

---

# Testing Recommendations

For each improvement:

1. **Unit Tests**: Test R6 classes and utility functions
2. **Integration Tests**: Test with actual Shiny modules
3. **UI Tests**: Test user workflows end-to-end
4. **Performance Tests**: Benchmark caching, logging overhead
5. **Backward Compatibility**: Ensure existing features still work

---

# Rollback Plan

Each improvement should include a rollback plan:
- Git branch with working version
- Feature flags to enable/disable new code
- Data migration scripts if needed
- Clear commit history for easy revert

---

# References

- [Developer Guide](maceR_Developer_Guide.pdf)
- [Shiny Best Practices](https://shiny.rstudio.com/articles/)
- [R6 Documentation](https://r6.r-lib.org/)
- [Design Patterns in R](https://adv-r.hadley.nz/oo.html)

---

**Last Updated**: April 15, 2026  
**Next Review**: After Phase 1 completion  
**Owner**: Development Team
