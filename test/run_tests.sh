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

# Parser errors
echo "one,two,three" | run parser_err view 'splitOn ,'

# Basic View
echo "one,two,three" | run basic_view view 'splitOn ","'

# Basic typechecking
echo "one,two,three" | run expected_list view 'splitOn "," | at 1'

# Splat
echo "one,two,three" | run splat_view view '[splitOn ","] | ...'

# 'At'
echo "one,two,three" | run at_view view '[splitOn ","] | at 1'


# Regex
echo "one two 555-123-4567 three" | run regex_view view '/[\d-]+/ | matches'
echo "one two 555-123-4567 three 999-876-5432" | run regex_modify modify '/[\d-]+/ | matches' '{rev}'
echo "one-two-three" | run regex_modify modify '/(\w+)-\w+-(\w+)/ | matches' '{tr a-z A-Z}'

# Regex Groups
echo "one two 555-123-4567 three" | run regex_groups_view view 'groups /(\d+)-(\d+)-(\d+)/'
echo "one two 555-123-4567 three" | run regex_groups_modify modify 'groups /(\d+)-(\d+)-(\d+)/' '{rev}'

# Filter
echo "one,two,three" | run filter_view view 'splitOn "," | filter /e/'

# Take/Drop
echo "one,two,three" | run take_view view 'take 2 (splitOn ",")'
echo "one,two,three" | run take_modify modify 'take 2 (splitOn ",")' '{rev}'
echo "one,two,three" | run drop_view view 'drop 1 (splitOn ",")'
echo "one,two,three" | run drop_modify modify 'drop 1 (splitOn ",")' '{rev}'

echo "one,two,three" | run take_end_view view 'takeEnd 1 (splitOn ",")'
echo "one,two,three" | run take_end_modify modify 'takeEnd 1 (splitOn ",")' '{rev}'
echo "one,two,three" | run drop_end_view view 'dropEnd 1 (splitOn ",")'
echo "one,two,three" | run drop_end_modify modify 'dropEnd 1 (splitOn ",")' '{rev}'

# Contains
echo "one,two,three" | run contains_view view 'splitOn "," | filter contains "two"'

# Template strings
# run template_view view 'match @"_-_-_" -> [one, two, three] '
# run template_view view 'groups @"_-_-_" -> [one, two, three] '

# Variable bindings
# echo "one,two,three" | run variable_binding modify 'splitOn "," | ->x' '{~ x }'



# Extract test cases from the readme

# focus --full modify 'groups /```focus\n(.*?)\n```/' 'take 1 lines | -{ echo %script }' README.md 
focus --full view 'groups /```focus\n(.*?)\n```/ | take 1 lines | -{ echo %script }' README.md 

