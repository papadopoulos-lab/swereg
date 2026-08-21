# Export a TTEPlan as a Slurm job chain

Writes one Slurm job script per stage, plus a driver script that chains
them with \`–dependency=afterok\`. This function never calls \`sbatch\`:
it writes files and returns their paths.

## Usage

``` r
tteplan_export_slurm(
  plan,
  repo,
  dir = ".",
  stages = c("s0_init.R", "s1.R", "s2.R", "s3.R", "s4_export.R"),
  cpus = 6,
  mem = "85G",
  no_requeue = "s1"
)
```

## Arguments

- plan:

  A \[TTEPlan\] with \`dir_tteplan_cp\` set.

- repo:

  Character(1). The analysis repository root that holds the stage
  scripts.

- dir:

  Character(1). The directory to write the \`slurm/\` sub-directory
  into. Defaults to the working directory.

- stages:

  Character vector of stage script paths, relative to \`repo\`, in
  submission order.

- cpus:

  Cores per stage. One unnamed value for every stage, or a vector naming
  every stage identity.

- mem:

  Memory per stage, in Slurm's own notation. One unnamed value for every
  stage, or a vector naming every stage identity.

- no_requeue:

  Character vector of stage identities to submit with \`–no-requeue\`.
  Use \`character(0)\` for none.

## Value

The written paths, invisibly, in submission order: the job scripts, then
the driver.

## Details

A \[TTEPlan\] already holds what a job chain needs. \`project_prefix\`
names the jobs, \`spec_version\` labels them, and \`expected_n_ids\`
with \`expected_skeleton_file_count\` give the progress denominators.
The caller supplies the two facts the plan does not hold: the repository
root, and the stage script names inside it.

## Generated files

The function writes into \`file.path(dir, "slurm")\` and creates that
directory when it is absent:

- \`\<prefix\>\_\<stage\>.sh\`:

  One job script per stage. It runs the mount check, calls
  \`check_version()\` on the plan, runs the stage script, and writes
  \`\<prefix\>\_\<stage\>\_mem.log\`.

- \`\<prefix\>\_submit.sh\`:

  The driver. It is the only generated file that names \`sbatch\`.

\`\<prefix\>\` is \`plan\$project_prefix\`, with each run of characters
outside \`\[A-Za-z0-9\_.-\]\` replaced by one \`\_\`. \`\<stage\>\` is
the stage identity, which is
\`tools::file_path_sans_ext(basename(stage))\`.

Every derived name MUST match \`^\[A-Za-z0-9\]\[A-Za-z0-9\_.-\]\*\$\`.
The leading character carries the rule. A name that starts with \`-\` is
a legal file name, and a command can read it as an option.

## Injection

A generated script is executable shell. A comment and a \`#SBATCH\`
directive both end at the first line break. A value that carries one
therefore escapes its line, and the rest of it becomes a command. The
function checks every value it embeds before it writes anything, and it
names the field it rejects.

Eleven fields reach generated text, under five rules:

- one line:

  \`project_prefix\`, \`spec_version\`, and the filesystem type \`stat\`
  reports.

- no whitespace at all:

  \`dir_tteplan\`, \`repo\`, \`dir\` and \`stages\`. These four are
  paths. A \`#SBATCH\` directive ends at the first whitespace character,
  and Slurm reads the rest as a separate option, so \`#SBATCH
  –output=/tmp/a b/x.out\` writes to \`/tmp/a\`.

- a whole number:

  \`expected_n_ids\` and \`expected_skeleton_file_count\`, or \`NULL\`,
  or \`NA\`.

- \`^\[0-9\]+\$\`:

  \`cpus\`.

- \`^\[0-9\]+\[KMGT\]?\$\`:

  \`mem\`.

The check covers a whole stage path, and not its file name alone. Take a
stage of \`x\<newline\>sbatch –wrap=true \#/s1.R\`. Its file name and
its identity are both valid, and it puts a live \`sbatch\` line into a
job script.

## Reuse

The exporter overwrites the files it generates. It removes no other
file, so \`file.path(dir, "slurm")\` MAY hold more files than this call
returns. Run it again with a stage removed or renamed, and that stage's
job script stays on disk. The driver names only the current stage set,
so the chain does not submit a stale script. Delete \`file.path(dir,
"slurm")\` first when you want the directory to hold this chain and
nothing else.

## What the generated chain guarantees

- One job per stage:

  \`sacct\` then reports each stage's own state, elapsed time and exit
  code.

- \`–no-requeue\` on a destructive stage:

  \`\$s1_generate_enrollments_and_ipw()\` deletes its work directory at
  startup. A requeue after a node failure restarts the script, which
  deletes the partial output and the evidence. Name that stage in
  \`no_requeue\`.

- \`–kill-on-invalid-dep=yes\` on every dependant:

  A job whose \`afterok\` dependency can never be satisfied otherwise
  sits in the queue forever as \`DependencyNeverSatisfied\`.

- A mount check inside the job:

  A queued stage can start days later, and an unmounted mount point is
  still a directory. The job reads a path under \`plan\$dir_tteplan\`
  first, then compares \`stat -f -c the filesystem type this function
  observed at generation time.

- Its own peak-memory reading:

  The job samples \`/sys/fs/cgroup/memory.peak\`, and falls back to
  \`VmHWM\` in \`/proc/self/status\`. It never reads the scheduler's
  accounting, which is empty on some builds.

- No path in the option stream:

  Every path a generated command takes positionally is absolute, apart
  from \`\$0\` in the driver. \`sbatch\`, \`cd\`, \`dirname\`, \`head\`,
  \`stat\`, \`cat\` and \`awk\` each take \`–\` as well.

## Errors

The function stops, and writes nothing, on any of these:

- \`stages\` is empty.

- Two stages share one identity.

- A stage script is absent under \`repo\`.

- A stage script file name does not match
  \`^\[A-Za-z0-9\]\[A-Za-z0-9\_.-\]\*\$\`.

- A derived job or file name does not match that same pattern.

- A stage identity is \`submit\`, which the driver reserves.

- Any embedded value holds a line break. See the Injection section.

- \`dir\`, \`repo\`, \`dir_tteplan\` or a stage path holds any
  whitespace character.

- \`cpus\` does not match \`^\[0-9\]+\$\`, or \`mem\` does not match
  \`^\[0-9\]+\[KMGT\]?\$\`.

- \`expected_n_ids\` or \`expected_skeleton_file_count\` is neither
  \`NULL\`, nor \`NA\`, nor one non-negative whole number.

- \`cpus\`, \`mem\` or \`no_requeue\` names a stage the chain does not
  hold.

- A named \`cpus\` or \`mem\` leaves a stage unnamed.

## See also

\[tteplan_locate_and_load()\], which every generated job script calls.
\`vignette("tte-workflow")\` describes the stage scripts this function
chains.

Other tte_plan:
[`registrystudy_load()`](https://papadopoulos-lab.github.io/swereg/reference/registrystudy_load.md),
[`tteplan_load()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_load.md),
[`tteplan_locate_and_load()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_locate_and_load.md)

## Examples

``` r
# The function reads the filesystem type with GNU `stat -f -c %T`, so this
# example runs on Linux only.
if (identical(Sys.info()[["sysname"]], "Linux")) {
  data_dir <- file.path(tempdir(), "slurm-example-data")
  repo <- file.path(tempdir(), "slurm-example-repo")
  out <- file.path(tempdir(), "slurm-example-out")
  dir.create(data_dir, showWarnings = FALSE)
  dir.create(repo, showWarnings = FALSE)
  file.create(file.path(data_dir, "tteplan.qs2"))
  file.create(file.path(repo, c("s0_init.R", "s1.R")))

  plan <- TTEPlan$new(
    project_prefix = "example",
    skeleton_files = "skeleton_001.qs2",
    global_max_isoyearweek = "2023-52"
  )
  plan$spec_version <- "v001"
  plan$dir_tteplan_cp <- CandidatePath$new(data_dir, "dir_tteplan")

  paths <- tteplan_export_slurm(
    plan,
    repo = repo,
    dir = out,
    stages = c("s0_init.R", "s1.R"),
    no_requeue = "s1"
  )
  basename(paths)
}
#> [1] "example_s0_init.sh" "example_s1.sh"      "example_submit.sh" 
```
