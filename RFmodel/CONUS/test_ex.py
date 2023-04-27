import os

slurm = int(os.environ['SLURM_ARRAY_TASK_ID'])

print(slurm)

for i in range(1,1000):
    print(f'loop {i}, jobarray index: {slurm}')
