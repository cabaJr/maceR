#' Messages 
#'
#' @description An object containing helper messages
#'
#' @return the content of the helper message to be displayed to the user in a 
#'     modalDialog
#'
#' @noRd

Messages <- R6::R6Class("Messages",
                    public = list(
                      help = list(textHelp_0_2 <- p(HTML("<h3>Load your files:</h3>
                                                              <br>
                                                              <ul>
                                                              <li>Multiple files containing your data need to be uploaded in the left input section.</li>
                                                              <li>File containing metadata about your group needs to be uploaded in the right input section.</li>
                                                              </ul>
                                                              <br>
                                                              Info about Data and Metadata organization can be found in <code>Data Structure</code> section.
                                                                                     ")),
                                  textHelp1_1_1 <- p(HTML("<ul>
                                                              <li>Set the rows to discard on top of each document (if the first rows do not contain data).</li>
                                                              <li>Select bewteen <code>Real time</code> (<i>YYYY-mm-dd HH:MM:SS</i>) or <code>Time points</code>.</li>
                                                              <li>Insert your timepoint duration in seconds (rate at which you have your data).</li>
                                                              <li>If you picked <code>Real time</code> fill the <mark>Experiment start day</mark> and <mark>Experiment start hour</mark> fields.</li>
                                                              <li>It's possible to subset up to two time windows, inserting start/end time point (in minutes, <code>Time point</code>option) or start/end Day AND hour (<code>Real time</code> option).</li>
                                                              </ul><br>
                                                              <mark>1 day = 1440 minutes</mark><br><br>
                                                              <mark>5 days = 7200 minutes</mark>
                                                                                      ")),
                                  textHelp1_1_2 <- p(HTML("If your data follow a 'common' timescale (independent from the single file) select <code>Yes</code>,
                                                              otherwise, by selecting <code>No</code>, you can decide how many rows of data to discard for each cabinet. <br>
                                                              Then select if the hour format in your metadata file is 12h or 24h.")),
                                  textHelp1_2_1 <- p(HTML("The standard format for metadata is the following: <br><br>
                                                              <img src = 'metaStd.png'; style = 'width:513px;height:180px';/><br><br>
                                                              It is possible to have different hour formats. If you want to include also minutes in your
                                                              Light_on column it is advisable to use the 24h format. <br><br>
                                                              If your metadata are not in the standard format you can select the <code>Custom</code> option to manually
                                                              select which columns contain which data.<br>
                                                              The most important values to set are: <mark>Id</mark>, <mark>Sex</mark>, <mark>Genotype</mark> and <mark>Cabinet</mark>. <br><br>
                                                              In addition you can create up to 3 custom factors to subset your data when creating plots.<br><br>
                                                              <mark>Example:</mark> if there is an <mark>Habituated</mark> factor in your metadata you can add it and use it to facet the actograms according to this variable.
                                                              ")),

                                  textHelp2_1 <- p(HTML("This is the table where <mark>metadata</mark> are displayed: <br>
                                                            By selecting <code>Display full metadata table</code> you will get a table containing all the records in your metadata file. <br>
                                                            By selecting <code>Display metadata of uploaded files only</code> the table will be filtered to show ONLY metadata related to the uploaded files.
                                                            ")),

                                  textHelp2_2 <- p(HTML("This is the table where your data are stored. It contains all the files uploaded through
                                                            <code>Input</code> > <code>Select files to analyse</code>. <br>
                                                            It is organised in a <a href='https://en.wikipedia.org/wiki/Wide_and_narrow_data'>Wide format</a>. <br>
                                                            It is possible to filter table content based on each column: <br><br><ul>
                                                            <li><mark>Id</mark>, <mark>Sex</mark>, <mark>Genotype</mark> and <mark>Cabinet</mark> (standard data structure)
                                                            are treated as <mark>factors</mark>, so they have to be directly typed in. You can find a list of the available
                                                            IDs in the <code>Input</code> page, or in the <code>Analysis</code> > <code>Subset data to visualize</code>, by ticking <code>Yes</code>. </li>
                                                            <li><mark>t</mark>(Time) and <mark>counts_min</mark> are discrete values and can be filtered using a slider or in alternative by typing the exact desired limits.</li>
                                                            <li>It is possible to change the number of rows to visualise (10, 25, 50, 100) using the drop menù in the top-left corner</li>
                                                            <li>The entire table of data can be downloaded in .csv format using the <code>Download</code> button in the bottom rigth position. </li></ul><br>
                                                            Filtering the table does not affect data plotting.
                                                            ")),

                                  textHelp3_0 <- p(HTML("By selecting <code>Yes</code> you can change the pool of data to use for generating plots, without having to
                                                            upload again the files that you want. <br><br>
                                                            The dropdown menu provide you the possibility to select individual <mark>IDs</mark> or groups based on <mark>Sex</mark>,
                                                            <mark>Genotype</mark> or <mark>Cabinet</mark>.<br>
                                                            It is possible also to select a group <b>AND</b> an <mark>ID</mark>, as well as multiple groups, the duplicates are not double printed. <br><br>
                                                            Selecting <code>No</code> all the data present in <code>Your Data</code> will be used for plotting.
                                                            ")),
                                  textHelp3_4 <- p(HTML("Activity onset is a function that allows you to detect the onset of activity per each day. <br>
                                                            at the end of the computation, a table will be created. Where the algorithm hasn't been able to find a value, a 0 will be put. <br>
                                                            The table is fully editable, you can change values in each cell at your will.<br><br>
                                                            <mark>Note:</mark> The algorithm still has to be optimised, so it is really slow if you upload multiple files, but the application hasn't freezed.
                                                            You can't use other functions while the computation is ongoing")),

                                  textHelp3_5 <- p(HTML("<h4>Function to create periodograms</h4><br><ul>
                                                            <li>Use the slider input to set the boundaries of your period identification.</li>
                                                            <li>Select the function to use from the drop down menu' on the right, <code>Chi square</code>,
                                                            <code>Autocorrelated</code> and <code>Lomb-Scargle</code> are the available ones.</li>
                                                            <li>Then select if you want to display lines for all IDs in the same plot (select <code>Cumulative</code>),
                                                            or if you want a separate plot for each ID (select <code>Individual</code>)</li> </ul> <br>
                                                            You will find the plots in the <code>Plot</code> section, inside the <mark>Periodogram</mark> Tab.
                                                            "))

                      ),
                      errors = list()
                    )
)
