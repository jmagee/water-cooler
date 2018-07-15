#!/bin/sh
# Convenience script for embedding water-cooler into the shell, similiar to
# thirsty.sh
# Source this file in your .{bash,zsh}rc file, then use the functions/commands
# defined below.

should_drink_water() {
  thirsty=$(water-cooler status)
  if [ ! -z "$thirsty" ]; then
    echo -n "$thirsty"
  fi
}

drink_water() {
  water-cooler drink $* | lolcat -a -d 30
}

not_thirsty() {
  water-cooler not-thirsty
}

out_of_water() {
  water-cooler no-water
}

next_drink() {
  water-cooler next
}

last_drink() {
  water-cooler last
}
