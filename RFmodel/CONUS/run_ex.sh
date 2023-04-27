#!/bin/bash
##SBATCH --nodelist=ceewater-cpu002 
#SBATCH --partition=gpu-preempt #gpu #ceewater_cjgleason-cpu # #cpu 
#SBATCH --nodes=1 
#SBATCH --ntasks-per-node=1 
#SBATCH --cpus-per-task=12 
#SBATCH --mem=32768 #64512  #
#SBATCH --time=1440 #4320 #2880 #in minutes 
#SBATCH --gpus=1
#SBATCH --gpus-per-task=1

#SBATCH --array=0-10
#SBATCH --output=./log/log_%A_%a.log 
#SBATCH --job-name=run_

#setup env
source ~/.bashrc
conda activate planetriv

#time stamp
date

#run code 
python3 test_ex.py 

#time stamp
date