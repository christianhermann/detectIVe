# Require Package (Special Rinno version with support for R 4 and higher https://github.com/bschneidr/RInno)
require(RInno)

Sys.setenv("TAR" = "internal")

# Build an installer
create_app(
  app_name = "DetectIVe2",
  publisher = "Christian Hermann",
  license_file = "About\\License.txt",
  compression = "bzip",
  info_after = "About\\Contact.txt",
  pkgs = c(
    "plotly",
    "shinyBS",
    "shinyalert",
    "shinybusy",
    "shinycustomloader",
    "shinyWidgets",
    "shinydashboard",
    "shinydashboardPlus",
    "shinyjs",
    "shiny",
    "rstudioapi",
    "tryCatchLog",
    "esquisse",
    "scales",
    "gtools",
    "dplyr",
    "tidyverse",
    "stringi",
    "vroom",
    "openxlsx",
    "gWidgets2",
    "matrixStats",
    "checkmate",
    "outliers",
    "purrrlyr",
    "ggsci",
    "ggthemes",
    "factoextra",
    "ggplot2",
    "patchwork",
    "cluster",
    "factoextra",
    "ggalt",
    "gtools",
    "latex2exp",
    "cowplot",
    "scales",
    "svglite",
    "Cairo",
    "gridExtra",
    "gWidgets2tcltk",
    "ggpubr",
    "showtext"
  ),
  remotes = "chrisstiann94/ephys.wsi",
  user_browser = "electron",
  app_desc       = "Processing, evaluation and analysis of patch clamp data",
                app_icon       = "www\\detectIVe2.ico",
                prog_menu_icon = T,
                desktop_icon   = T)
#For package "estimability an older version needs to be downloaded and installed by hand. Atleast for r.4.2.3
#Change iss file after:
#Under Icon, change commondesktop to userdesktop.
#Also change the following part to the uncommented one to make sure R 4.2.3 is used.:
#success := false;
#// for v := 0 to (RVersions.Count) do
#//    begin
#//      if RegKeyExists(HKLM, 'Software\R-Core\R\' + RVersions[v]) or RegKeyExists(HKCU, 'Software\R-Core\R\' + RVersions[v]) then
#//      begin
#//        success := true;
#//        RRegKey := 'Software\R-Core\R\' + RVersions[v];
#//        break;
#//      end;
#//   end;
#  if RegKeyExists(HKLM, 'Software\R-Core\R\4.2.3') or RegKeyExists(HKCU, 'Software\R-Core\R\4.2.3') then
#      begin
#      success := true;
#      RRegKey := 'Software\R-Core\R\4.2.3';
#      end;
#  Result := success;
#end;



compile_iss()
