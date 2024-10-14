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

## Splat
echo "one,two,three" | run splat_view view '[splitOn ","] | ...'

## 'At'
echo "one,two,three" | run at_view view '[splitOn ","] | at 1'


## Regex
echo "one two 555-123-4567 three" | run regex_view view '/[\d-]+/'
echo "one two 555-123-4567 three 999-876-5432" | run regex_modify modify '/[\d-]+/' '{rev}'
echo "one-two-three" | run regex_modify modify '/(\w+)-\w+-(\w+)/' '{tr a-z A-Z}'

### Regex Groups
echo "one two 555-123-4567 three" | run regex_groups_view view 'groups /(\d+)-(\d+)-(\d+)/'
echo "one two 555-123-4567 three" | run regex_groups_modify modify 'groups /(\d+)-(\d+)-(\d+)/' '{rev}'

## Filter
echo "one,two,three" | run filter_view view 'splitOn "," | filter /e/'

## Take/Drop
echo "one,two,three" | run take_view view 'take 2 (splitOn ",")'
echo "one,two,three" | run take_modify modify 'take 2 (splitOn ",")' '{rev}'
echo "one,two,three" | run drop_view view 'drop 1 (splitOn ",")'
echo "one,two,three" | run drop_modify modify 'drop 1 (splitOn ",")' '{rev}'

echo "one,two,three" | run take_end_view view 'takeEnd 1 (splitOn ",")'
echo "one,two,three" | run take_end_modify modify 'takeEnd 1 (splitOn ",")' '{rev}'
echo "one,two,three" | run drop_end_view view 'dropEnd 1 (splitOn ",")'
echo "one,two,three" | run drop_end_modify modify 'dropEnd 1 (splitOn ",")' '{rev}'

## Contains
echo "one,two,three" | run contains_view view 'splitOn "," | filter (contains "two")'
echo "one,two,three" | run contains_modify modify 'splitOn "," | filter (contains "two")' '{rev}'

## Not
echo "one,two,three" | run not_view view 'splitOn "," | filter (not (contains "two"))'
echo "one,two,three" | run not_modify modify 'splitOn "," | filter (not (contains "two"))' '{rev}'

## JSON
echo '{"one": 1, "two": 2, "three": 3}' | run json_view view 'json'

## Casting
echo '1,2,3' | run requires_cast view '[ splitOn "," ] | (at 0) + (at 1)'
echo '1,2,3' | run successful_cast view '[ splitOn "," ] | (at 0) + (at 1)'

# Expressions

## Binding usages
echo "The password is swordfish" | run binding_usage modify '/The password is (?<password>\w+)/' '%password'

## Binding usages in template strings
echo "The password is swordfish" | run binding_usage_in_template view '/The password is (?<password>\w+)/ | "password: %{ %password | {rev}}!"'

## Pattern strings
echo "The password is swordfish" | run pattern_string view 'pattern "The password is %password" | "password: %password"'
echo '[http://google.ca](My Link)' | run pattern_modify modify 'pattern "[%descr](%link)"' '"[%link](%descr)"'

## String Concatenation
echo "one,two,three" | run string_concat_view view '[splitOn ","] | concat %.'

## Intersperse
echo "1 2 3 one two three" | run intersperse_view view 'intersperse /\d+/ /\w+/ (splitOn " ")'

## Binding assignment
echo 'one,two,three' | run binding_assignment_view view 'splitOn "," | -> x | concat ["*", %x, "*"]'

## Count
echo 'one,two,three' | run count_view view 'count (splitOn ",")'

## Math

### Int Math

#### Plus
echo '1,2,3' | run plus_view view '[ splitOn "," ] | !(at 0) + !(at 1)'

#### Minus
echo '10,8' | run minus_view view '[ splitOn "," ] | !(at 0) - !(at 1)'

#### Times
echo '10,8' | run times_view view '[ splitOn "," ] | !(at 0) * !(at 1)'

#### Division
echo '10,5' | run division_view view '[ splitOn "," ] | !(at 0) / !(at 1)'

#### Modulus
echo '10,3' | run modulus_view view '[ splitOn "," ] | !(at 0) % !(at 1)'

#### Power
echo '2,3' | run power_view view '[ splitOn "," ] | !(at 0) ^ !(at 1)'

### Mixed Math

#### Plus
echo '1.5,2' | run plus_mixed_view view '[ splitOn "," ] | !(at 0) + !(at 1)'

#### Minus
echo '10,8.5' | run minus_mixed_view view '[ splitOn "," ] | !(at 0) - !(at 1)'

#### Times
echo '10,8.5' | run times_mixed_view view '[ splitOn "," ] | !(at 0) * !(at 1)'

#### Division
echo '10,2.5' | run division_mixed_view view '[ splitOn "," ] | !(at 0) / !(at 1)'

#### Modulus
echo '10,3.5' | run modulus_mixed_view view '[ splitOn "," ] | !(at 0) % !(at 1)'

#### Power
echo '2,3.5' | run power_mixed_view view '[ splitOn "," ] | !(at 0) ^ !(at 1)'

## Expressions in modify commands
echo '1,2' | run expression_in_selector modify '[ splitOn "," ] | !(at 0) + !(at 1)' '%.'

# Extract test cases from the readme

# focus --full view '/```focus\n((.|\s)*?)\n```/ | drop 1 (dropEnd 1 lines) | filter (not (contains "Result")) | -{ echo %{.} }' README.md
# focus --full view '"```focus\n_\n```" | drop 1 (dropEnd 1 lines) | filter (not (contains "Result")) | . + "\nOutput: " -{ echo %{.} }' README.md

# focus --full view '"```focus\n_\n```" | drop 1 (dropEnd 1 lines) | filter (not (contains "Result")) | . + "\nOutput: " -{ echo %{.} }' README.md
