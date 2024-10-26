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
  selector="$2"
  arg="$3"
  rendered_cmd="focus '$selector' '$arg'"

  divider "COMMAND" "$out_file"
  echo "$rendered_cmd" >> "$out_file"


  input_file=$(mktemp)
  cat > "$input_file"

  divider "INPUT" "$out_file"
  cat "$input_file" >> "$out_file"

  divider "OUTPUT" "$out_file"
  { <"$input_file"  "$focus" --no-color "$selector" 2>&1 | ansifilter >> "$out_file"
    exit_code="$?"
  } || true

  divider "EXIT CODE" "$out_file"
  echo "$exit_code" >> "$out_file"
}

"$focus" --help > "help.out"

# Parser errors
echo "one,two,three" | run parser_err 'splitOn ,'

# Basic View
echo "one,two,three" | run basic_view 'splitOn ","'

# Basic typechecking
echo "one,two,three" | run expected_list 'splitOn "," | at 1'

## Splat
echo "one,two,three" | run splat_view '[splitOn ","] | ...'

## 'At'
echo "one,two,three" | run at_view '[splitOn ","] | at 1'


## Regex
echo "one two 555-123-4567 three" | run regex_view '/[\d-]+/'
echo "one two 555-123-4567 three 999-876-5432" | run regex_modify '/[\d-]+/ |= {rev}'
echo "one-two-three" | run regex_modify '/(\w+)-\w+-(\w+)/ |= {tr a-z A-Z}'

### Regex Groups
echo "one two 555-123-4567 three" | run regex_groups_view 'groups /(\d+)-(\d+)-(\d+)/'
echo "one two 555-123-4567 three" | run regex_groups_modify 'groups /(\d+)-(\d+)-(\d+)/ |= {rev}'

## Filter
echo "one,two,three" | run filter_view 'splitOn "," | filter /e/'

## Take/Drop
echo "one,two,three" | run take_view 'take 2 (splitOn ",")'
echo "one,two,three" | run take_modify 'take 2 (splitOn ",") |= {rev}'
echo "one,two,three" | run drop_view 'drop 1 (splitOn ",")'
echo "one,two,three" | run drop_modify 'drop 1 (splitOn ",") |= {rev}'

echo "one,two,three" | run take_end_view 'takeEnd 1 (splitOn ",")'
echo "one,two,three" | run take_end_modify 'takeEnd 1 (splitOn ",") |= {rev}'
echo "one,two,three" | run drop_end_view 'dropEnd 1 (splitOn ",")'
echo "one,two,three" | run drop_end_modify 'dropEnd 1 (splitOn ",") |= {rev}'

## Contains
echo "one,two,three" | run contains_view 'splitOn "," | filter (contains "two")'
echo "one,two,three" | run contains_modify 'splitOn "," | filter (contains "two") |= {rev}'

## Not
echo "one,two,three" | run not_view 'splitOn "," | filter (not (contains "two"))'
echo "one,two,three" | run not_modify 'splitOn "," | filter (not (contains "two")) |= {rev}'

## JSON
echo '{"one": 1, "two": 2, "three": 3}' | run json_view 'json'

## Casting
echo '1,2,3' | run requires_cast '[ splitOn "," ] | (at 0) + (at 1)'
echo '1,2,3' | run successful_cast '[ splitOn "," ] | (at 0) + (at 1)'

# Expressions

## Binding usages
echo "The password is swordfish" | run binding_usage '/The password is (?<password>\w+)/ |= %password'

## Binding usages in template strings
echo "The password is swordfish" | run binding_usage_in_template '/The password is (?<password>\w+)/ | "password: %{ %password | {rev}}!"'

## Pattern strings
echo "The password is swordfish" | run pattern_string 'pattern "The password is %password" | "password: %password"'
echo '[http://google.ca](My Link)' | run pattern_modify 'pattern "[%descr](%link)" |= "[%link](%descr)"'

## String Concatenation
echo "one,two,three" | run string_concat_view '[splitOn ","] | concat %.'

## Intersperse
echo "1 2 3 one two three" | run intersperse_view 'intersperse /\d+/ /\w+/ (splitOn " ")'

## Binding assignment
echo 'one,two,three' | run binding_assignment_view 'splitOn "," | -> x | concat ["*", %x, "*"]'

## Count
echo 'one,two,three' | run count_view 'count (splitOn ",")'

## Math

### Int Math

#### Plus
echo '1,2,3' | run plus_view '[ splitOn "," ] | !(at 0) + !(at 1)'

#### Minus
echo '10,8' | run minus_view '[ splitOn "," ] | !(at 0) - !(at 1)'

#### Times
echo '10,8' | run times_view '[ splitOn "," ] | !(at 0) * !(at 1)'

#### Division
echo '10,5' | run division_view '[ splitOn "," ] | !(at 0) / !(at 1)'

#### Modulus
echo '10,3' | run modulus_view '[ splitOn "," ] | !(at 0) % !(at 1)'

#### Power
echo '2,3' | run power_view '[ splitOn "," ] | !(at 0) ^ !(at 1)'

### Mixed Math

#### Plus
echo '1.5,2' | run plus_mixed_view '[ splitOn "," ] | !(at 0) + !(at 1)'

#### Minus
echo '10,8.5' | run minus_mixed_view '[ splitOn "," ] | !(at 0) - !(at 1)'

#### Times
echo '10,8.5' | run times_mixed_view '[ splitOn "," ] | !(at 0) * !(at 1)'

#### Division
echo '10,2.5' | run division_mixed_view '[ splitOn "," ] | !(at 0) / !(at 1)'

#### Modulus
echo '10,3.5' | run modulus_mixed_view '[ splitOn "," ] | !(at 0) % !(at 1)'

#### Power
echo '2,3.5' | run power_mixed_view '[ splitOn "," ] | !(at 0) ^ !(at 1)'

## Expressions in modify commands
echo '1,2' | run expression_in_selector '[ splitOn "," ] | !(at 0) + !(at 1) |= %.'

# Extract test cases from the readme

# focus --full '/```focus\n((.|\s)*?)\n```/ | drop 1 (dropEnd 1 lines) | filter (not (contains "Result")) | -{ echo %{.} }' README.md
# focus --full '"```focus\n_\n```" | drop 1 (dropEnd 1 lines) | filter (not (contains "Result")) | . + "\nOutput: " -{ echo %{.} }' README.md

# focus --full '"```focus\n_\n```" | drop 1 (dropEnd 1 lines) | filter (not (contains "Result")) | . + "\nOutput: " -{ echo %{.} }' README.md
#
#
# focus --full '/```focus\n(?<script>(.|\s)*?)\n```/ |= %script | drop 1 (dropEnd 1 lines) | filter (not (contains "Result")) | -{ echo %{.} }' README.md
