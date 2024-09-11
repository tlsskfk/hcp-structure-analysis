#!/bin/bash
#SBATCH -n 1
#SBATCH -t 7-0
#SBATCH -c 16
#SBATCH --mem-per-cpu=4096
#SBATCH --oversubscribe

module purge
#module switch umd-software-library/new
module load openmpi
#module load r/4.3.2/gcc/11.3.0/linux-rhel8-zen2
module load r

WDIR="/scratch/zt1/project/jpurcel8-prj/shared"
R_LIBS=$WDIR/R_libs
CMDSTAN=$WDIR/.cmdstan/cmdstan-2.35.0

export R_LIBS
export CMDSTAN
export SLURM_JOB_ID

date 
echo "Running $R_SCRIPT..."

echo "Make a temporary directory for the job"
mkdir -p /tmp/$SLURM_JOB_ID
cp -R $WDIR/* /tmp/$SLURM_JOB_ID/
cd /tmp/$SLURM_JOB_ID

#Rscript --save ./SLURM_R_SCRIPT$script.R
pwd
Rscript --save ../Code/$1.R

echo "finished running Code $1.R"
date

#cp /tmp/$SLURM_JOB_ID/*.rds $WDIR/$R_DIR

# You will need this if you want to send it to another server
#echo "scp $WDIR/$R_DIR/*.rds $WDIR/$R_DIR/*.txt $SCP_OUTPUT_SERVER" >> $WDIR/$R_DIR/scp.sh

#sbalance
#date
#echo "This is the last 25 lines of output from the slurm job associated with $CUR_RSCRIPT" >> $WDIR/$R_DIR/$tmpwd.out
#echo "tail -n 25 $WDIR/output-$SLURM_JOB_ID.out >> $WDIR/$R_DIR/$tmpwd.out"
