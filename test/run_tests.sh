#!/bin/bash

set -e
# set -x

unset FOCUS_DEBUG

# cd to the directory of the script
cd "$(dirname "$0")/output" || exit 1

rm -r ./*.out 2>/dev/null || true

# Run the tests
stack build --fast > /dev/null
focus=$(stack exec -- which focus)

divider() {
  echo "===$1===============================================================================" >> "$2"
}

run() {
  out_file="$1.out"
  cmd="$2"
  selector="$3"
  arg="$4"
  rendered_cmd="focus $cmd '$selector' '$arg'"

  divider "COMMAND" "$out_file"
  echo "$rendered_cmd" >> "$out_file"


  input_file=$(mktemp)
  cat > "$input_file"

  divider "INPUT" "$out_file"
  cat "$input_file" >> "$out_file"

  divider "OUTPUT" "$out_file"
  case "$cmd" in
    "view")
      { <"$input_file"  "$focus" --no-color "$cmd"  "$selector" 2>&1 | ansifilter >> "$out_file"
        exit_code="$?"
      } || true
      ;;
    "modify" | "set")
      { <"$input_file"  "$focus" --no-color "$cmd" "$selector" "$arg"  2>&1 | ansifilter >> "$out_file"
        exit_code="$?"
      } ||  true
      ;;
    *)
      echo "Unknown cmd: $cmd"
      exit 1
      ;;
  esac

  divider "EXIT CODE" "$out_file"
  echo "$exit_code" >> "$out_file"
}

"$focus" --help > "help.out"

# Extract test cases from the readme

# focus --full modify 'groups /```focus\n(.*?)\n```/' 'take 1 lines | -{ cat %script }' README.md 

# Parser errors
echo "one,two,three" | run parser_err view 'splitOn ,'

# Basic View
echo "one,two,three" | run basic_view view 'splitOn ","'

# Basic typechecking
echo "one,two,three" | run expected_list view 'splitOn "," | at 1'

# Splat
echo "one,two,three" | run splat view '[splitOn ","] | ...'

# 'At'
echo "one,two,three" | run at view '[splitOn ","] | at 1'


# Regex
echo "one two 555-123-4567 three" | run regex_view view '/[\d-]+/ | matches'
echo "one two 555-123-4567 three 999-876-5432" | run regex_modify modify '/[\d-]+/ | matches' '{rev}'
echo "one-two-three" | run regex_modify modify '/(\w+)-\w+-(\w+)/ | matches' '{tr a-z A-Z}'

# Regex Groups
echo "one two 555-123-4567 three" | run regex_groups_view view 'groups /(\d+)-(\d+)-(\d+)/'
echo "one two 555-123-4567 three" | run regex_groups_modify modify 'groups /(\d+)-(\d+)-(\d+)/' '{rev}'

# Filter
echo "one,two,three" | run simple_filter view 'splitOn "," | filterBy /e/'

# Variable bindings
# echo "one,two,three" | run variable_binding modify 'splitOn "," | ->x' '{~ x }'
