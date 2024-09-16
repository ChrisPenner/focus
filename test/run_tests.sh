#!/bin/bash

set -e
# set -x

unset FOCUS_DEBUG

# cd to the directory of the script
cd "$(dirname "$0")/output" || exit 1

rm ./*.out || true

# Run the tests
stack build --fast > /dev/null
focus=$(stack exec -- which focus-exe)

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
      { "$focus" --no-color "$cmd"  "$selector" < "$input_file" >> "$out_file" 2>&1;
        exit_code="$?"
      } || true
      ;;
    "modify" | "set")
      { "$focus" --no-color "$cmd" "$selector" "$arg" < "$input_file" >> "$out_file" 2>&1  
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

# Regex
echo "one two 555-123-4567 three" | run regex_view view '/[\d-]+/'
echo "one two 555-123-4567 three 999-876-5432" | run regex_modify modify '/[\d-]+/ | matches' '{rev}'
echo "one-two-three" | run regex_modify modify '/(\w+)-\w+-(\w+)/ | matches' '{tr a-z A-Z}'
