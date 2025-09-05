# Create the target directory if it doesn't exist
mkdir -p ./static/images

# Read the entire JSON input
json_data=$(cat)

# Extract the hits array and process each hit
echo "$json_data" | jq -c '.hits[]' | while read -r hit; do
    id=$(jq -r '.id' <<< "$hit")
    
    # Process each URL type
    for type in previewURL webformatURL largeImageURL; do
        url=$(jq -r ".$type" <<< "$hit")
        if [ "$url" != "null" ] && [ -n "$url" ]; then
            filename="${id}_${type}.jpg"
            filepath="./static/images/$filename"
            
            # Download the image
            curl -o "$filepath" "$url"
            
            # Update the JSON with the new relative path
            hit=$(jq --arg type "$type" --arg filename "/static/images/$filename" ".$type = \$filename" <<< "$hit")
        fi
    done
    
    # Output the modified hit
    echo "$hit"
done | jq -s . | jq --argjson original "$json_data" '. as $hits | $original | .hits = $hits'
