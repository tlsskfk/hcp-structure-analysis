#!/bin/bash
#SBATCH -n 1
#SBATCH -t 7-0
#SBATCH -c 16
#SBATCH --mem-per-cpu=4096
#SBATCH --oversubscribe

module purge
module load openmpi
module load r

W_DIR="/scratch/zt1/project/jpurcel8-prj/shared/hcp-structure-analysis"
R_LIBS=$W_DIR/../R_libs
CMDSTAN=$W_DIR/../.cmdstan/cmdstan-2.35.0
CSV_DIR=$W_DIR/03-transformed-csv
CSV=data_brms_log_normalized.RDS
MODEL_DIR=$W_DIR/04-models-sh
ZARATAN_DIR=$W_DIR/05-zaratan-sh
OUTPUT_DIR=$W_DIR/06-bayes-results
SCP_OUTPUT_SERVER=$USER@jude.umd.edu:/data/jude/FAD/hcp-structure-analysis/06-bayes-results

date 
echo "Running model $R_SCRIPT..."

# 1. Make a temporary directory for the job"
mkdir -p /tmp/$SLURM_JOB_ID

# 2. Copy over necessary things
cp $CSV_DIR/$CSV /tmp/$SLURM_JOB_ID/

# 3. Make R script
cp $MODEL_DIR/main.R /tmp/$SLURM_JOB_ID/$1
cat $MODEL_DIR/$1 >> /tmp/$SLURM_JOB_ID/$1

# 4. cd
cd /tmp/$SLURM_JOB_ID

# 5. Run script and echo script for acknowledgement
echo "Running the following script: "
echo "------------------------------------------------------"
cat ./$1
echo ""
echo "------------------------------------------------------"
echo ""
echo "Running..."

Rscript --save ./$1

# 6. Move over result
rm ./$CSV
rm ./$1

OUTPUT_FILE=$(ls ./*) 
cp "./$OUTPUT_FILE" "$OUTPUT_DIR/"

echo "Finished running model $1"
echo "The time is now: "
date
echo ""

# 7. Echo scp statement
cp $ZARATAN_DIR/$SLURM_JOB_ID.out $OUTPUT_DIR/$OUTPUT_FILE.txt
echo "scp $OUTPUT_DIR/$OUTPUT_FILE $ZARATAN_DIR/$OUTPUT_FILE.txt $SCP_OUTPUT_SERVER"