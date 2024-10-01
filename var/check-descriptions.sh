find "${1}" -iname "*.md" | sort | while read -r _path; do
  _description="$(yq --front-matter=extract .description "${_path}")"
  echo "${_description}"
  if [ "${_description}" = "null" ]; then
    echo "No description found in ${_path}"
    exit 0
  fi
done
