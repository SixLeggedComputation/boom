boom 0.0.1 is a crash manager. I wanted one, to be used with several of my apps and I also wanted to assay racket GUI capability. This is a first attempt.

boom will be a customizable third-party application, which desktop applications will be able to run before crashing, so as to inform users, display error reports, eventually send these to a bug tracker (though contacting remote bug trackers is not implemented yet) and restart calling application.

# First, a little of vocabulary

- caller: application, which issued a crash report and invoked boom
- crash report: data file, which describes caller fault.
- prompt: human readable message displayed by the popup window, which informs the user, about caller crash. This message is distinct from the crash report.

#Operating systems
Ubuntu, Windows.

#Distribution

##Files in this distribution

- source code (*.rkt)
- localization files (*.rktd)
- configuration file (local.cfg)
- dummy crash report for testing (crash.json)

## Files not in this distribution

boom depends on packages:

- config
- starter

# Compilation
Compilation was tested under ubuntu and windows. In Dr Racket, select menu Racket > Create Executable, then select GRacket. Or in terminal, type: raco exe --gui --orig-exe boom.rkt

# Usage

##Customizing boom

A default configuration is hard-coded. It can be overridden in 4 ways. A per caller app configuration can be achieved by providing a distinct configuration file. A per bug configuration can be obtained by toggling command-line options or when generating crash report file. In the end, some options can be enabled/disabled by toggling associated fields in UI. Precedence order is summarized in the following (where '>' means 'overrides'):

user choices > bug report > command line > config file > default

A few style parameters are defined in config file and default configuration only.

A number of icons is needed for buttons or options selection lists. These are expected to be stored in one directory. A default set of icons is provided, but this can be personalized in config file by providing an other directory.

boom logs its own warnings and error messages to standard error channel.

### Crash report file

Providing error report is mandatory. Error reports are passed by the caller as JSON-encoded key value pairs. Crash report file location can be provided as a commad line argument. In case it is not, boom looks for a file, that is named "crash.json" in its executable directory. You can replace "crash.json" with your own file. When no command line argument is provided and no crash.json is found, a warning is displayed and restarting the caller is disabled.

The following table lists available keys. These are case-sensitive and must be double-quoted. String-associated values are double-quoted too. Numbers are unquoted. Any other key, but those provided hereunder, is silently ignored.

|Key              |Type          |Description                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    |
|-----------------|--------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|**time**             |64-bit integer|Bug Unix epoch                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 |
|**software**         |string        |Caller app name. Content of this field is intended for display only. It is not used for restarting caller app. For this, see "restart". Leaving it blank causes no issue. if so, "restart" field is checked and its value is used in replacement, if provided. But this may yield cryptic displays.                                                                                                                                                                                                                                                                                                                                                                                           |
|**version**          |string           |Caller app version. Typically major + minor + revision. But can be any other string. Only used for display. Can be left blank.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  |
|**description**      |string        |Bug report content. Mandatory. It shoud be a string. But a report consisting only of a number is valid. A report consisting of only the following symbols #t or #f is not. Avoid SQL statements. They are not detected at the moment, but they should be prohibited in future updates. If this field is missing or is invalid, the report window displays "No information was provided".                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
|**enable-tracking**|boolean       |disables/enables sending reports to a bug tracker. Use "tracker" field for providing URL.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      |
|**tracker**        |url           |Reporting URL. To be used only when enable-tracking is true. Otherwise, an empty string can be provided : "". If an empty string is provided while "enable-tracking" is true, tracking is disabled. Remote reporting is not yet implemented.                                                                                                                                                                                                                                                                                                                                                                                                            |
|**restart**        |string        |                                                                                                                                                                                                   Command line to be used for restarting caller application. Can be left blank                                                                                                                                                                                                                                                                                                                                |
|**lq**             |string        |                                                                                                                                                                                                                                                                              Command line to be used when the report should be appended to a local bug tracker. Both "tracker" and "lq" fields can be provided, or just one of them or none. This option is not implemented yet.                                                                                                                                                                   |
|**dau**              |boolean       |                                                                                                                                                                                                                                                                                                                                                                                                                                           Should the report file be destroyed after having been displayed? True by default. If another report with same name is emitted, it will crush the previous one anyway|
|                 |              |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               |


###Command line

Only the most basic configuration parameters have been implemented so far. Some more will be added. A default configuration is hard-coded, so that all command line parameters are optional.

    boom [-r crash-report] [-c file][-d b][-i f][-s b]

crash-report is a path or URL (not implemented yet). If not provided, boom will look either for a "crash.json" file in its executable directory or at the directory place pointed to by its settings.

|Key|Type|Overrides|Allowed values|What it does|What happens when wrong?|
|---|-----| |--------------|------------|----------------------|
|**c**  |string| |paths or URL                   |Provides an alternative config file.|defaults to original config|
|**d**|boolean |destroy-after-usage|false (= f, #f, 0) or true (anything else)|if true, crash report file is erased, once used unless user choses to keep it. if false, crash report file is not erased|defaults to original config (= true). End decision is made by user in any case|
|**i**|string  |main-icon|paths or URL|Use this this flag, in order to customize main form and have it display an icon of your own. Downloading from remote URLs is not implemented yet. For the moment, path must point to a valid image file. Following formats are supported: bmp, gif, jpeg, png.|The file is rejected. An alert is logged.|
|**r**|string  |standard-report-file|paths or URL|Provides an alternative location for loading crash report. Download from remote location is not implemented yet. File must exists and be json|An error message is displayed in main window and an alert is logged|
|**s**|boolean |show-summary|false (= f, #f, 0) or true (=anything else)|If true, summary window is disabled and does not show at the end|Defaults to config file value|
|**v**|boolean|report-at-start|false (= f, #f, 0) or true (=anything else)|When true, the popup window displays crash report data by default on opening|If no proper crash report file is provided and -v is true, the popup window displays a message, which informs the user, it does not know by which app it has been called. There is no other context, in which things could go wrong, since any value that is not a false is interpreted as true|

###Config file

Config file name extension is *.cfg. Config files consist in key value pais in the form of a series ofencoded into S expressions. Case sensitive parameter name comes first (left column in table). It is followed by its value, which can be a double-quoted string literal, an integer or a float. Booleans are #t (true) and #f (false). Parameters ordering does not matter. None of them is mandatory. But if you change resources-icons, you will have to consider changing resources-tasks accordingly.


|Parameter name          |Value type  |Overriden by|What it does                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         |
|------------------------|------------|------------|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|**destroy-after-usage**|boolean|-d|if true, crash report is erased after boom exectution|
|**display-left-margin**|integer| |Style parameter. Number of pixels. Vertical spacing to containers top border|
|**display-margins**|integer| |Style parameter. Number of pixels. Spacing to containers right and bottom margins|
|**display-top-margin**  | Integer    | |                                                                                                                                                                                                                                                Style parameter. Vertical spacing of form components. Value in pixels.                                                                                                                                                                                          |
|**head-font-face**|String| |Either a platform independent font family or a font face. Accepted platform independent families are: decorative, roman, script, swiss, modern, symbol, system. If a font face is provided, it must name an installed font.|
|**head-font-size**      |Double      | |Prompt font size. Any value outside of range 4..60 will cause font size to default to 12.0 and a message will be sent to log channel.                                                                                                                                                                                                                                                                                                                                                                                     |
|**main-icon**|String|-i|Path to main icon file. This is the icon, that is shown next to prompt. Refer to -i for details|
|**resources-icons**     |String    |   |                                                                                                                                                                                                                                                                                                                                                                                                                                              Path to all icons files, except main form prompt image. If let empty, value will default {exe]/resources/icons, {exe} is binary directory. This directory must also provide resources for summary window in a subdirectory, the name of which must be provided through resources-tasks.|
|**report-at-start**|boolean|-v|Set this parameter to #t for the popup window to display crash report on opening. Setting it to #f will hide the crash report on window opening, but the user will still be able to display it by clicking on a button|
|**resources-tasks**|String| |Specific icons set for summary window. This field must point to a subdirectory of resource-icons|
|**show-summary**|boolean|-s|Should an operation summary window be opened before boom terminates|
|**standard-report-file**|String     |-r|Path ending with *.json extension. Change this value to tell the software, where to look for bug report. When it is a disk location, check you have access privilege. Remote locations URL not supported yet.                                                                                                                                                                                                                                                                                                                                                                           |

## Restarting caller app

Caller is in charge of providing a valid command line. Boom sends it to the operating system. For security reasons, each time boom does that, an entry is created in a separate text-formatted diary, so that users can check what is started by boom and when. It is named 'restart.txt' and is located in the executable directory. This automatic restart can always be disabled from UI.

# todos

- contact remote bug tracker
- add corporate icon
- improve UI
- turn config file parameters into lists, so as to manage app-specific profiles
- making boom able to be contacted by remote crashing computers
- filter-out SQL statements from report
- manage recovery schemes
