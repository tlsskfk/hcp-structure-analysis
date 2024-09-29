#!/bin/bash


W_DIR="/scratch/zt1/project/jpurcel8-prj/shared/hcp-structure-analysis"
MODEL_DIR=$W_DIR/04-models-sh
SBATCH_SH=$W_DIR/05-zaratan-sh/sbatch.sh

export USER

# 1. List all files in the MODEL_DIR
files=($(find "$MODEL_DIR" -maxdepth 1 -type f ! -name "main.R"))
echo "Available files (excluding 'main.R' and folders):"
i=1
for file in "${files[@]}"; do
    echo "$i) $(basename "$file")"
    ((i++))
done

# 2. Prompt the user to select a file
echo "Select a file by number:"
read -p "Enter the number of the file you want to run with sbatch: " selection

# 3. Check if the selection is a valid number and within range
if [[ "$selection" -gt 0 && "$selection" -le "${#files[@]}" ]]; then
    selected_file="${files[$selection-1]}"
    filename=$(basename "$selected_file")
    echo "You selected: $filename"
    
    # 4. Run the SBATCH_SH script with the filename (not the full path) as the argument
    echo "Running sbatch with $filename..."
    sbatch "$SBATCH_SH" "$filename"
else
    echo "Invalid selection. Exiting."
fi