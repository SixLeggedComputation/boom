boom 0.0.1 is a crash manager. I wanted one, to be used with several of my apps and I also wanted to assay racket GUI capability. This is a first attempt. This proof of concept works. But fitting content into UI containers turned out to be trickier than expected. At the moment backend is more satisfying than front-end.

boom will be a customizable third-party application, which desktop applications will be able to run before crashing, so as to inform users, display error reports, eventually send these to a bug tracker (though contacting remote bug trackers is not implemented yet) and restart calling application.

# First, a little of vocabulary

Hereunder 'caller' refers to any application, which sends crash reports and involes boom.

#Files in this distribution

- source code (*.rkt)
- localization files (*.rktd)
- configuration file (local.cfg)
- dummy crash report for testing (crash.json)

# Compilation command
Compilation was tested in ubuntu. Type: raco exe --gui --orig-exe boom.rkt

# Usage

##Customizing boom

A default configuration is hard-coded. It can be overridden in 4 ways. A per caller app configuration can be achieved by providing a distinct configuration file. A per bug configuration can be obtained by toggling command-line options or when generating crash report file. In the end, some options can be enabled/disabled by toggling associated fields in UI. Precedence order is summarized in the following (where '>' means 'overrides'):

user choices > bug report > command line > config file > default

### Crash report file

Error reports are passed by the caller as key value pairs following JSON protocole. Crash report file location can be provided as a commad line argument. In case it is not, boom looks for a file, that is named "crash.json" in its executable directory. You can replace "crash.json" with your own file. When no command line argument is provided and no crash.json is found, a warning is displayed. Restarting the caller is disabled.

Any other key but those provided hereunder are ignored.

|Key              |Type          |Description                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    |
|-----------------|--------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|time             |64-bit integer|Bug Unix epoch                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 |
|software         |string        |Caller app name. Content of this field is intended for display only. It is not used for restarting caller app. For this, see "restart". Leaving it blank causes no issue. if so, "restart" is checked and used in replacement, if provided. This can yield ugly or uninformative displays.                                                                                                                                                                                                                                                                                                                                                                                           |
|version          |string           |Caller app version. Typically major + minor + revision. But can be any other string. Only used for display. Can be left blank.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  |
|description      |string        |Bug report content. Mandatory. It shoud be a string. But a report consisting only of a number is valid. A report consisting of only the following symbols #t or #f is not. Avoid SQL statements. They are not detected at the moment, but they should be prohibited in future updates. If this field is missing or is invalid, the report window displays "No information was provided".                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
|"enable-tracking"|boolean       |disables/enables sending reports to a bug tracker. Use "tracker" field for providing URL.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      |
|"tracker"        |url           |Reporting URL. To be used only when enable-tracking is true. Otherwise, an empty string can be provided : "". If an empty string is provided while "enable-tracking" is true, tracking is disabled. Remote reporting is not yet implemented.                                                                                                                                                                                                                                                                                                                                                                                                            |
|"restart"        |string        |                                                                                                                                                                                                   Command line to be used for restarting caller application. Can be left blank                                                                                                                                                                                                                                                                                                                                |
|"lq"             |string        |                                                                                                                                                                                                                                                                              Command line to be used when the report should be appended to a local bug tracker. Both "tracker" and "lq" fields can be provided, or just one of them or none. This option is not implemented yet.                                                                                                                                                                   |
|dau              |boolean       |                                                                                                                                                                                                                                                                                                                                                                                                                                           Should the report file be destroyed after having been displayed? True by default. If another report with same name is emitted, it will crush the previous one anyway|
|                 |              |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               |


###Command line

Only the most basic configuration parameters have been implemented so far. Some more will be added. A default configuration is hard-coded, so that all command line parameters are optional.

boom [-r crash-report] [-c file][-d b][-i f][-s b]

crash-report is a path or URL (not implemented yet). If not provided, boom will look either for a "crash.json" file in its executable directory or at the directory place pointed to by its settings.

|key|Type|Allowed values|What it does|What happens if wrong?|
|---|----|--------------|------------|----------------------|
|c  [string|paths or URL|Provides an alternative config file.|defaults to original config|
|d|boolean|false (= f, #f, 0) or true (anything else)|if true, crash report file is erased, once used unless user choses to keep it. if false, crash report file is not erased|defaults to original config (= true). End decision is made by user in any case|
|i|string[paths or URL|Use this this flag, in order to customize main form and have it display an icon of your own. Downloading from remote URLs is not implemented yet. For the moment, path must point a valid image file. Following formats are supported: bmp, gif, jpeg, png.|The file is rejected. An alert is logged.|
|r|string|paths or URL|Provides an alternative location for loading crash report. Download from remote location is not implemented yet. File must exists and be json|An error message is displayed in main window and an alert is logged|
|s|boolean|false (= f, #f, 0) or true (=anything else)|If true, summary window is disabled and does not show at the end|Defaults to config file value|

## Restarting caller app

Caller is in charge of providing a valid command line. Boom sends it to the operating system. For security reasons, each time boom does that, an entry is created in a separate text-formatted diary, so that users can check what is started by boom and when. It is named 'restart.txt' and is located in the executable directory. This automatic restart can always be disabled from UI.

# todos

- Check Windows configuration
- contact remote bug tracker
- add corporate icon
- rework UI
- turn config file parameters into lists, so as to manage app-specific profiles
- making boom able to be contacted by remote crashing computers
- filter-out SQL statements from report
