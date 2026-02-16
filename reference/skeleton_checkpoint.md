# Create a profiling checkpoint closure

Factory function that returns a closure for recording time and memory
usage at each step of skeleton processing. Call the closure with a label
at each step boundary. Call with \`done = TRUE\` to finalize and return
a data.table of all checkpoints.

## Usage

``` r
skeleton_checkpoint()
```

## Value

A function with signature \`function(label = NULL, done = FALSE)\`: -
Call with a label string to record a checkpoint - Call with \`done =
TRUE\` to return a data.table with columns \`label\`, \`time\`, and
\`mem_mb\`

## See also

Other skeleton_utils:
[`skeleton_save()`](https://papadopoulos-lab.github.io/swereg/reference/skeleton_save.md)

## Examples

``` r
if (FALSE) { # \dontrun{
cp <- skeleton_checkpoint()
cp("start")
# ... do work ...
cp("STEP 1: Select IDs")
# ... more work ...
cp("STEP 2: Create skeleton")
prof_dt <- cp(done = TRUE)
# prof_dt is a data.table with columns: label, time, mem_mb
} # }
```
