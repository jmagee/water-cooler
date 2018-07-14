![WaterCooler](img/water-cooler-logo.png)

Overview
========
`water-cooler` is a tool to remind you to drink water.  It is ~~inspired by~~ a
blatant rip-off of [thirsty.sh](https://github.com/kalbhor/thirsty).

`thirsty.sh` is nice tool - I found it tremendously improved my water
consumption throughout the day.  However, being a simple shell script, it had
some limitations - namely that it wasn't convenient to integrate into non-shell
environment (non-shell environment? _pure blasphemy_!)

Screenshot
==========
Coming soon.

Installation
============
 1. Install stack (https://docs.haskellstack.org/en/stable/README/)
 2. `stack config`  -- first time after stack installation only
 3. `stack build`
 4. `stack test`    -- optional, if you want to run the tests
 5. `stack install` -- optional, if you want install it

Usage
=====

Check if it is time for a drink
-------------------------------
`$ water-cooler status`

If it is time for a drink, this command will print "You're thirsty."  Otherwise
it will print nothing and simply return.

This command can be embedded into your shell prompt/status, window manager, or
anywhere else that can call a program.

Drink some water
----------------
`$ water-cooler drink [optional drink size]`

Drink size can be one of:

* `sip` - A small drink
* `swallow` - An average size drink
* `gulp` - A big drink
* `fake` - You didn't really drink, but you want to lie to the program that you did.

The optional flag `--wait N` can be used to set a custom time, where N is the
number of seconds until the next drink.
The default wait time until the next drink is 1200 seconds, or 20 minutes.

Check time until the next drink
-------------------------------
`$ water-cooler next`

The time until the next drink is displayed in seconds.

Check time since last drink
---------------------------
`$ water-cooler last`

The last drink size and time is display.  The default format of the time is
`YYYY-MM-DD HH:MM:SS`.  To override, see the option `--env-timeformat`.

You are not thirsty
-------------------
`$ water-cooler not-thirsty`

Indicates you are not thirsty and will reset the drink reminder.
The next drink reminder will be issued in 600 seconds, or 10 minutes.

The optional flag `--wait N` can be used to override the number of seconds
until the next drink reminder.

Out of water
------------
`$ water-cooler no-water`

Indicates that you are out of water.  water-cooler will suggest getting a
refill and the next drink reminder will be issued in 3600 seconds, or 1 hour.

The optional flag `--wait N` can be used to override the number of seconds
until the next drink reminder.

Options
=======

| Option                       | Purpose            |
|------------------------------|--------------------|
|`--help`                      | Display help       |
|`--version`                   | Display version    |
| `--wait N`                   | Set a custom value for the number of seconds for the next drink.  Effects only `drink`, `not-thirsty`, and `no-water` commands. |
|`--env-cooler`                | Specify a custom cooler file, must be an absolute path.  |
|`--env-history`               | Specify a custom history file, must be an absolute path. |
|`--env-sip-text` text         | Specify a message to display after sipping.    |
|`--env-swallow-text` text     | Specify a message to display after swallowing. |
|`--env-gulp-text` text        | Specify a message to display after gulping. |
|`--env-fake-text` text        | Specify a message to display when skipping a drink. |
|`--env-empty-text` text       | Specify a message to display when out of water. |
|`--env-thirsty-text` text     | Specify a message to display when thirsty. |
|`--env-timeformat` format-text| Specify the Unix-style date/time format string. |

Shell usage
===========
Embedding `water-cooler` within a shell prompt can be achieved via a
command such as `$(water-cooler status)`.  For convenience, a shell script is
provided in the [scripts](scripts) directory which implements an interface
similar to that provided by thirst.sh.  Simple source
`scripts/water-cooler-script.sh` to make use of these shell functions.

Shell completion
----------------
Shell completion can be enabled for different shells.  For example:

```
$ source <(water-cooler --bash-completion-script `which water-cooooler`)
```

Normally, the output of --bash-completion-script would be saved to a file and
stored in a completion directory (e.g. `/etc/bash_completion.d`.)

Completion options:

 * `--bash-completion-script`
 * `--zsh-completion-script`
 * `--fish-completion-script`

Customization
=============
It is possible to customize some aspects of `water-cooler`.
This can be done via command line options or via a RC file.

For example, let's suppose one desired to see the text "Way to go" after swallows
and gulps, and the text "That's not enough!" after sips.
This could be achieved via command line options:

`water-cooler drink sip --env-sip-text "That's not enough"`

`water-cooler swallow sip --env-swallow-text "Way to go"`

`water-cooler gulp sip --env-swallow-text "Way to go"`

Alternatively, the preferred default can be saved in a RC file of the form:
`$HOME/.water-cooler.rc`

This can be created and populated automatically with the `mkrc` command:
```
water-cooler mkrc --env-sip-text "That's not enough" \
                  --env-swallow-text "Way to go" \
                  --env-gulp-text "Way to go"
```

The resulting RC file can be manually edited.

The RC file is in the following JSON format:
```
{
    "drinkText": [
        "This is the sip text",
        "This is the swallow text",
        "This is the gulp text",
        "This is the not thirsty text",
        "This is the out of water text?"
    ],
    "history": "This must be an absolute path to the cooler file"
    "cooler": "This must be an absolute path to the history file"
    "thirstyText": "This is the text when thirsty"
    "timeFormat": "Unix-style time format string"
}
```

To generate/see a RC file with the default values, simply run `water-cooler mkrc`.

Acknowledgements
================
The original idea is from [Thirsty](https://github.com/kalbhor/thirsty), which
is a great tool.  If `water-cooler` seems a bit too heavy weight, I'd strongly
suggest giving thirsty a try.
