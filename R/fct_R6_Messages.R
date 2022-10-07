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
                      landing = list(
                        landing_left <- p(HTML("<h3>Landing left</h3>
                                                    <br>
                                                    <ul>
                                                    <li>Multiple files containing your data need to be uploaded in the left input section.</li>
                                                    <li>File containing metadata about your group needs to be uploaded in the right input section.</li>
                                                    </ul>
                                                    <br>
                                                    Info about Data and Metadata organization can be found in <code>Data Structure</code> section.
                                                                           ")),
                        landing_main <- p(HTML("<h3>Landing main</h3>
                                                    <br>
                                                    <ul>
                                                    <li>Multiple files containing your data need to be uploaded in the left input section.</li>
                                                    <li>File containing metadata about your group needs to be uploaded in the right input section.</li>
                                                    </ul>
                                                    <br>
                                                    Info about Data and Metadata organization can be found in <code>Data Structure</code> section.
                                                                           ")),
                        landing_right <- p(HTML("<h3>Landing right</h3>
                                                    <br>
                                                    <ul>
                                                    <li>Multiple files containing your data need to be uploaded in the left input section.</li>
                                                    <li>File containing metadata about your group needs to be uploaded in the right input section.</li>
                                                    </ul>
                                                    <br>
                                                    Info about Data and Metadata organization can be found in <code>Data Structure</code> section.
                                                                           "))
                      ),
                      help = list(
                        textHelp_0_2 <- p( #1
                          HTML(
                          "<h3>Load your files</h3>
                            <br>
                            <ul>
                            <li>Multiple files containing your data must be uploaded in the left input section.</li>
                            <li>File containing metadata about your group needs to be uploaded in the right input section.</li>
                            </ul>
                            <br>
                            If you are unsure how to format your metadata file press the <code>Create metadata manually</code> button 
                            to see the right formatting. 
                            <br>
                            You can type in or paste your metadata into the table from an external spreadsheet and download the correctly
                            formatted <mark> .csv </mark> file to upload in the metadata input section.
                        ")
                        ),
                        textHelp1_1_1 <- p( #2
                        HTML("
                        <h3>Data files settings</h3>
                        <br>
                        <ul>
                        <li>Set the rows to discard on top of each document (if the first rows do not contain data).</li>
                        <li>Select bewteen <code>Real time</code> (<i>YYYY-mm-dd HH:MM:SS</i>) or <code>Time points</code>.</li>
                        <li>Insert your timepoint duration in seconds (rate at which you have collected your data).</li>
                        <li>If you picked <code>Real time</code> fill the <mark>Experiment start day</mark> and <mark>Experiment start hour</mark> fields.</li>
                        </ul><br>
                        <mark>1 day = 1440 minutes</mark><br><br>
                        <mark>5 days = 7200 minutes</mark>
                             ")
                        ),
                        textHelp1_1_12 <- p( #3
                          HTML("
                        <h3>Light/Dark cycle</h3>
                        <br>
                        This optional section allows you to set up details on the light/dark schedule of your experiment.
                        If you want to include this info in the plots that will be generated tick <code>Yes</code> and fill in the values
                        <ul>
                        <li>Set the lenght of your light period. Day lenght is fixed at 24 hours so the dark period will result from the difference between 24 and your input value. </li>
                        <li>Set the constant darkness (DD) startday. This is the moment in the experiment when you switched to constant darkness</li>
                        </ul><br>
                             ")
                        ),
                        textHelp1_1_13 <- p( #4
                          HTML("
                        <h3>Load data</h3>
                        <br>
                        This button will load the uploaded files and parameters into o dataframe for further analysis and data plotting.
                        <br>
                        When the computation is complete you will be taken to the next section where you can explore you data and chosse between several the analyses.
                               ")
                        ),
                        textHelp1_1_2 <- p( #5
                          HTML(
                          "If your data follow a 'common' timescale (independent from the single file) select <code>Yes</code>,
                          otherwise, by selecting <code>No</code>, you can decide how many rows of data to discard for each cabinet. <br>
                          Then select if the hour format in your metadata file is 12h or 24h.
                          ")
                          ),
                        textHelp1_2_1 <- p( #6
                          HTML(
                          "The standard format for metadata is the following: <br><br>
                          <img src = 'metaStd.png'; style = 'width:513px;height:180px';/><br><br>
                          It is possible to have different hour formats. If you want to include also minutes in your
                          Light_on column it is advisable to use the 24h format. <br><br>
                          If your metadata are not in the standard format you can select the <code>Custom</code> option to manually
                          select which columns contain which data.<br>
                          The most important values to set are: <mark>Id</mark>, <mark>Sex</mark>, <mark>Genotype</mark> and <mark>Cabinet</mark>. <br><br>
                          In addition you can create up to 3 custom factors to subset your data when creating plots.<br><br>
                          <mark>Example:</mark> if there is an <mark>Habituated</mark> factor in your metadata you can add it and use it to facet the actograms according to this variable.
                          ")
                          ),

                        textHelp2_1 <- p( #7
                          HTML(
                          "This is the table where <mark>metadata</mark> are displayed: 
                          <br>
                          By selecting <code>Display full metadata table</code> you will get a table containing all the records in your metadata file. 
                          <br>
                          By selecting <code>Display metadata of uploaded files only</code> the table will be filtered to show ONLY metadata related to the uploaded files.
                          ")),

                        textHelp2_2 <- p( #8
                          HTML(
                          "This is the table where your data are stored. It contains all the files uploaded through
                          <code>Input</code> > <code>Select files to analyse</code>. <br>
                          It is organised in a <a href='https://en.wikipedia.org/wiki/Wide_and_narrow_data'>Narrow (long) format</a>. <br>
                          It is possible to filter table content based on each column: <br><br><ul>
                          <li><mark>Id</mark>, <mark>Sex</mark>, <mark>Genotype</mark> and <mark>Cabinet</mark> (standard data structure)
                          are treated as <mark>factors</mark>, so they have to be directly typed in. You can find a list of the available
                          IDs in the <code>Input</code> page, or in the <code>Analysis</code> > <code>Subset data to visualize</code>, by ticking <code>Yes</code>. </li>
                          <li><mark>t</mark>(Time) and <mark>counts_min</mark> are discrete values and can be filtered using a slider or in alternative by typing the exact desired limits.</li>
                          <li>It is possible to change the number of rows to visualise (10, 25, 50, 100) using the drop menu in the top-left corner</li>
                          <li>The entire table of data can be downloaded in .csv format using the <code>Download</code> button in the bottom rigth position. </li></ul><br>
                          Filtering the table does not affect data plotting.
                          ")),

                        textHelp3_1 <- p( #9
                          HTML(
                          "<h3>Data subsetting</h3>
                          <br>
                          By selecting <code>Yes</code> you can change the pool of data to use for generating plots, without having to
                          upload again the files that you want. 
                          <br>
                          <br>
                          <h4>Animals subsetting</h4>
                          <br>
                          The dropdown menu provide you the possibility to select individual <mark>IDs</mark> or groups based on <mark>Sex</mark>,
                          <mark>Genotype</mark> or <mark>Cabinet</mark>.
                          <br>
                          It is possible also to select a group <b>AND</b> an <mark>ID</mark>, as well as multiple groups, the duplicates are not double printed. 
                          <br>
                          <br>
                          <h4>Time subsetting</h4>
                          <br>
                          Drag the slider bar to use only a portion of your data to generate the plots
                          <br>
                          <br>
                          Selecting <code>No</code> all the data present in <code>Your Data</code> will be used for plotting.
                          ")),
                        textHelp3_2 <- p( #10
                          HTML(
                            "<h3>Creating Double plotted actograms</h3>
                          <br>
                          
                          ")
                        ),
                        textHelp3_3 <- p( #11
                          HTML(
                            "<h3>Creating single line actograms</h3>
                          <br>
                          
                          ")
                        ),
                        textHelp3_4 <- p( #12
                          HTML(
                            "<h3>Creating Sum of daily activity plots</h3>
                          <br>
                          
                          ")
                        ),
                        textHelp3_5 <- p( #13
                          HTML(
                          "<h4>Creating periodograms</h4><br><ul>
                          <li>Use the slider input to set the boundaries of your period identification.</li>
                          <li>Select the function to use from the drop down menu' on the right, <code>Chi square</code>,
                          <code>Autocorrelated</code> and <code>Lomb-Scargle</code> are the available ones.</li>
                          <li>Then select if you want to display lines for all IDs in the same plot (select <code>Cumulative</code>),
                          or if you want a separate plot for each ID (select <code>Individual</code>)</li> </ul> <br>
                          You will find the plots in the <code>Plot</code> section, inside the <mark>Periodogram</mark> Tab.
                          ")
                          ),
                        textHelp3_6 <- p( #14
                          HTML(
                            "<h3>Creating Average day plots</h3>
                          <br>
                          
                          ")
                        )
                        

                      ),
                      errors = list()
                    )
)
 
# HTML(
#   "Activity onset is a function that allows you to detect the onset of activity per each day. <br>
#                           at the end of the computation, a table will be created. Where the algorithm hasn't been able to find a value, a 0 will be put. <br>
#                           The table is fully editable, you can change values in each cell at your will.<br><br>
#                           <mark>Note:</mark> The algorithm still has to be optimised, so it is really slow if you upload multiple files, but the application hasn't freezed.
#                           You can't use other functions while the computation is ongoing
#                           ")