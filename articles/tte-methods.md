# TTE methods: manuscript and statistical analysis plan text

This vignette gives methods text for the target trial emulation (TTE)
pipeline in swereg (`TTEEnrollment`, `TTEPlan` and related classes). It
covers the intention-to-treat (ITT) and the per-protocol (PP) estimand.
It has four sections:

- **Statistical analysis plan** (Section 1). The values each study
  states, the formulas, model specifications, conventions, identifying
  assumptions and known limitations. It follows the target trial table
  of the TARGET guideline. It names no code, so a statistician can
  reimplement the estimators from it. The one exception is 1.8.9, which
  names the three time columns. Copy it whole into a protocol, a
  pre-registered statistical analysis plan or a methods supplement. The
  section numbers stay valid.
- **Manuscript methods** (Section 2). Short past-tense text for the
  methods section of a journal article, in the order of Section 1.
  Replace the placeholders that Section 2 lists.
- **Validation evidence** (Section 3). The design of the validation
  battery, the data-generating processes, and tables and figures of
  estimate against truth for every validation cell. The numbers are
  rendered from a saved results file.
- **Implementation mapping** (Section 4). The function, argument,
  option, column and test file behind each step of Section 1, and notes
  on changes to the estimators.

Section 1 describes what the code computes. Where the implementation
differs from a standard reference construction, Section 1 states the
difference and the reason. The implementation follows the sequential
trial emulation literature (Hernán et al. 2008; Danaei et al. 2013;
Hernán and Robins 2016; Caniglia et al. 2023; Cashin et al. 2025).

------------------------------------------------------------------------

## 1. Statistical analysis plan

This section specifies the estimators as swereg implements them. It
names no code, so a statistician can reimplement the estimators from it.
Sections 1.1 to 1.8 follow the rows of the target trial table in TARGET
items 6 and 7 (Cashin et al. 2025). Sections 1.9 and 1.10 add the
sensitivity analyses and the limitations. The validation documentation
of the software reports the simulation evidence for its quantitative
statements.

### Values your protocol must state

Each study states the values below in its protocol. Each item names the
value that swereg uses when the specification is silent, or says that
there is none.

- The width of each trial’s enrollment period. Default: 4 weeks. The
  same width sets each follow-up interval (1.4).
- The new-user washout window. Default: no washout. swereg then warns
  that no washout covers the intervention (1.1).
- The sampling ratio for comparators. No default: the specification
  states it, for example 2:1, or swereg stops (1.3).
- The analysis horizon and the administrative end of study. No default
  horizon: the specification lists each one. The administrative end
  defaults to the last week in the first batch of the data (1.4).
- The tolerance for each arm. Default: 0 weeks in each arm, so the first
  discordant week is a deviation (1.4).
- The confounders in the treatment weight model and in the censoring
  weight model. No default. One list of confounders serves both models
  (1.8.1, 1.8.2).
- The truncation percentiles. Default: the 1st and 99th percentiles,
  which the pipeline holds fixed (1.8.3).
- Subgroups, and how heterogeneity is tested. Default: no subgroups.
  Each subgroup gets rate ratios within its levels and a Wald test of
  its interaction with treatment (1.8.8).
- Missing data, and how it is handled. Default: the rules of 1.8.7. They
  include a single hot-deck imputation of a missing confounder value at
  entry.

### 1.1 Eligibility criteria

TARGET items 6a and 7a.

Calendar time is cut into consecutive enrollment periods of equal width.
Each enrollment period opens one trial. Eligibility is assessed in every
week of the data. A person can therefore be eligible in some weeks of an
enrollment period and not in others.

The study states global criteria: a range of ISO years, inclusion
criteria and exclusion criteria. Each enrollment can add its own age
range, range of ISO years, inclusion criteria and exclusion criteria.
These apply after the global criteria. An enrollment takes one age range
and one range of ISO years at most.

An enrollment’s range of ISO years limits trial entry week by week. An
enrollment period that crosses a new year therefore recruits from its
in-range weeks only. Follow-up continues past the end of the range. The
range lies inside the global range of ISO years.

A look-back window counts the calendar ISO weeks before the current
week, not the data rows. A week with no data row still counts toward the
window. An annual record covers every ISO week of its year. A lifetime
window covers every earlier week. The current week is never inside its
own window.

One further exclusion window reads every week of the person, after
baseline as well. It removes a person from every trial because of a
later event, so eligibility then depends on the future.

swereg imposes no new-user rule of its own. A washout exclusion on the
treatment history makes the design a new-user design. Its window is a
fixed number of weeks, for example 104 weeks as in Danaei et al. (2013).
For a never-user design, the window is the whole earlier history.

A lifetime washout lets each person start treatment in one enrollment
period at most. Every later week is then ineligible, so the person
leaves all later trials. A finite washout expires. It stops excluding
the person once its window holds no week at the washed-out value.

A protocol without any washout enrolls prevalent users in the
intervention arm at every enrollment period. People who stop treatment
can re-enter as comparators. That is a prevalent-user design, and it is
rarely the intended estimand.

swereg warns when no washout covers the intervention. A prevalent week
is a week at the intervention value after an earlier week of the same
person at that value. A washout covers the enrollment when it makes
every prevalent week ineligible. A washout on a parent column can
therefore cover a sub-type arm, and a washout on the right column at the
wrong value cannot. The check reads the weekly rows of the first data
batch only, so it misses a counterexample that exists only in a later
batch.

### 1.2 Treatment strategies

TARGET items 6b and 7b.

The protocol compares two strategies, the intervention and the
comparator. One column of the weekly data holds the treatment of each
week. The protocol names the value of that column that marks each arm. A
week that holds neither value is outside both arms.

Under the per-protocol estimand, a person follows the assigned strategy
while each week holds the value of the assigned arm. Section 1.4 states
when a departure ends follow-up.

One value per week cannot record two treatments in the same week. The
washout exclusions cannot resolve that case either, because each one
reads earlier weeks only. The protocol decides the rule when it builds
the column. A week coded as missing drops out of the arm classification,
and it has further effects under some washouts (1.8.7).

### 1.3 Assignment procedures

TARGET items 6c and 7c.

Arm classification in an enrollment period reads only the weeks in which
the person is eligible and holds one of the two arm values. It drops
every other week first. A person enters the intervention arm when at
least one of those weeks holds the intervention value. A person enters
the comparator arm when all of them hold the comparator value. A
candidate person-trial with no such week is ineligible for that trial.

A week outside both arms therefore does not prevent a comparator
classification. The recruiting week is the earliest week of the
enrollment period that is eligible and holds an arm value. The treatment
weights read each confounder at the recruiting week (1.8.1).

The worked example below shows the filter. A protocol compares
intervention A with comparator B. One column holds the treatment of each
week, and it reads A, B or missing. Two washout exclusions apply to both
arms: no earlier A, and no earlier B. Each one covers the whole earlier
history. One person starts B in week 1 of a four-week enrollment period,
and starts A in week 3.

| Week | Treatment | Eligible | Why                          |
|------|-----------|----------|------------------------------|
| 1    | B         | yes      | no earlier week holds A or B |
| 2    | missing   | no       | week 1 holds B               |
| 3    | A         | no       | week 1 holds B               |
| 4    | A         | no       | week 1 holds B               |

The classification keeps week 1 alone. That week holds the comparator
value, so the person enters the comparator arm, and the recruiting week
is week 1. The two A weeks change nothing, because the filter already
dropped them.

The current week is outside its own look-back window (1.1). The week of
a first start therefore stays eligible. Under a lifetime washout on both
arms, each person enters the arm of the treatment they started first. No
separate rule is needed for a person who starts both treatments in one
enrollment period.

All intervention person-trials of an enrollment period are enrolled.
Comparators enter by incidence density sampling from the same enrollment
period, with a seed stated in the specification. A specification without
a seed is refused, so every draw can be reproduced. The draw takes the
comparator-to-intervention ratio times the number of intervention
person-trials, rounded to a whole number. Where fewer qualified
comparators remain, the draw takes all of them.

The draw runs after the time-zero checks of 1.4, so it refills the ratio
from qualified comparators alone. It is stratified by the enrollment
period, not by the week, and it reads no other variable. It attaches no
comparator to an intervention person-trial, so it forms no matched set,
and no later step conditions on one. The weights carry all confounding
adjustment (1.8.1). The draw bounds computation.

### 1.4 Follow-up

TARGET items 6d and 7d.

Time zero is a landmark: the first week after the enrollment period
closes. A person enters the trial only if they reach that week under
observation and free of every enrollment outcome.

swereg checks both conditions on each candidate person-trial before the
comparator draw. The outcome check reads every outcome of the
enrollment, over the whole earlier history of the person. An outcome in
the time-zero week is a follow-up event. The enrollment period therefore
contributes no follow-up and no immortal time (Caniglia et al. 2023).
Its width sets how long a newly eligible person waits for the next time
zero.

Each enrolled person-trial is split into follow-up intervals of the same
width as the enrollment period. Their number is the horizon divided by
the width, rounded up. The first interval opens at time zero, and the
last one is clipped at the horizon. Follow-up time counts weeks from
time zero.

For each person-trial, follow-up stops at the first of these events:

1.  the first outcome at or after time zero;
2.  loss of observation: the first week after time zero without
    observation, whether a gap in observation or the end of the record;
3.  under the per-protocol estimand only, a protocol deviation;
4.  the administrative end of study;
5.  the analysis horizon.

A protocol deviation stops per-protocol follow-up at the start of the
first discordant week beyond the tolerance of the arm. That week is not
per-protocol follow-up, so an outcome in it or later is not counted,
even in the same follow-up interval. The row that reaches the deviation
is clipped there and marked as censored by deviation. This is the rule
of Danaei et al. (2013, p. 77) and of `expand_until_switch()` in
TrialEmulation (Su et al. 2024). Releases 26.9.0 to 26.15.0 counted the
deviation week as follow-up. Releases 26.7.3 to 26.15.0 also counted an
outcome later in the follow-up interval of the deviation.

A gap is a week without observation between two observed weeks. It stops
follow-up under both estimands at the start of its first absent week,
and no tolerance applies. An outcome in that week or later is never
counted, even in the same follow-up interval. The row that reaches the
gap is clipped there and marked as censored by loss of observation.

Two stops can fall on the same week. One rule then labels the row:

- an outcome counts, and the row is not censored;
- an administrative end or the horizon is complete follow-up, so a loss
  or a deviation on the same week does not censor;
- a loss of observation is labelled loss and not deviation, because a
  person who is not observed cannot be seen to deviate.

Every stop is exact to the week. None is rounded to the edge of a
follow-up interval. A trial whose time zero falls after the
administrative end contributes no follow-up.

Under the per-protocol estimand, deviation is read from the weekly
records. A week is discordant when it does not hold the value of the
assigned arm. A week with a missing treatment value and a week outside
both arms are discordant in both arms.

Each arm has a tolerance $k$, the number of consecutive discordant weeks
it allows. A concordant week resets the run. The deviation falls at the
start of week $(k + 1)$ of a run of consecutive discordant weeks. A
tolerance of 0 therefore places it at the start of the first discordant
week.

A run that starts before time zero counts only its weeks from time zero.
An absent week breaks a run. No grace period applies, and the pipeline
does not clone person-trials. A treatment deviation never stops
follow-up under the intention-to-treat analogue.

Rows at and before the stop are kept. The row that reaches the stop is
clipped at that week, so it holds the person-time before the stop and
none after it. The outcome sits at its exact week: the row that holds it
is clipped there and carries the event. Person-time is the stop minus
the start, in weeks, on a half-open interval. A time-updated confounder
takes its value in the first week of the interval. The loss and
deviation models of 1.8.2 fit on the clipped rows. A deviation at time
zero leaves the person-trial no row, and the time-zero model of 1.8.2
keeps it in the weights.

### 1.5 Outcomes

TARGET items 6e and 7e.

Each outcome is a weekly indicator in the data, and the protocol names
the primary one. Every combination of enrollment, outcome and horizon is
one emulated trial, analysed on its own. Follow-up counts the first
occurrence of the outcome at or after time zero. An earlier occurrence
of any outcome of the enrollment excludes the candidate person-trial
(1.4).

The risk is cause-specific. Death and the end of observation censor
follow-up, and no competing-risk model is fitted (1.6).

### 1.6 Causal contrasts

TARGET items 6f and 7f.

Both estimands condition on reaching time zero under observation and
free of every enrollment outcome. The target of inference is therefore a
landmark-survivor estimand. It says nothing about people who die, or
have an outcome, before time zero (Dafni 2011). Both estimands are
marginal: the weights standardise them over the baseline confounder
distribution of the enrolled person-trials.

- The intention-to-treat analogue is the effect of the intervention
  versus the comparator strategy as assigned in the enrollment period,
  ignoring later changes of treatment. The treatment weight alone
  estimates it.
- The per-protocol estimand is the effect of sustained intervention
  versus the sustained comparator strategy. Follow-up is censored at a
  deviation, when a run of discordant weeks exceeds the arm’s tolerance
  (1.4). The product of the treatment and censoring weights estimates
  it.

Each estimand is reported on two scales. The relative scale is the
marginal incidence rate ratio (IRR) of 1.8.4. The absolute scale is the
risk difference at follow-up week $t$:

$${RD}(t) = \{ 1 - S_{1}(t)\} - \{ 1 - S_{0}(t)\} = S_{0}(t) - S_{1}(t).$$

Here $S_{a}(t)$ is the weighted survival of arm $a$ through week $t$
(1.8.5). Arm $a = 1$ is the intervention and arm $a = 0$ the comparator.
A protective intervention gives a negative risk difference. The risk
difference is estimated at every distinct stop time on the weekly grid,
and reported at the end of follow-up.

The number needed to treat is $- 1/{RD}(t)$. A positive value is the
number needed to treat for benefit (NNTB). A negative value is reported
by its magnitude as the number needed to treat for harm (NNTH) (Altman
1998). A risk difference of exactly zero has no number needed to treat.

The risk is the net risk implied by the cause-specific hazard of the
outcome. It describes a world without death only if removing death would
not change that hazard. It is not the cumulative incidence with death as
a competing risk. It is at least as large as the cumulative incidence
computed from the same hazards.

The IRR is the coefficient of a proportional-rates working model. The
marginal rate ratio can change over follow-up, for example through
depletion of susceptible people or an effect that accumulates. The IRR
is then a person-time-weighted average of the time-varying rate ratio.
That average can differ from the ratio of the risks at the end of
follow-up. In the simulations of section 3, swereg and TrialEmulation
estimate the same weighted average under strongly time-varying effects.

The risk-difference curve shows how the effect develops over follow-up.
Report it beside the IRR when a time-varying effect is of scientific
interest.

### 1.7 Identifying assumptions

TARGET items 6g, 7g.i and 7g.ii.

The intention-to-treat analogue rests on four assumptions:

1.  consistency;
2.  no unmeasured confounding of assignment, given the confounders at
    the recruiting week;
3.  positivity of assignment within confounder strata;
4.  loss to follow-up that is independent of the outcome.

No censoring weights apply to the intention-to-treat analysis. In the
simulations of section 3, the estimand holds under independent loss and
is biased under informative loss, in swereg and TrialEmulation alike.

The per-protocol estimand rests on two further assumptions:

5.  the censoring models of 1.8.2 capture all common causes of deviation
    or loss and of the outcome, including their time-varying values in
    the data;
6.  positivity of continued adherence.

The variables behind assumptions 2 and 5 are the confounders that the
protocol lists. Assumption 2 reads them at the recruiting week.
Assumption 5 reads their most recent value in each follow-up interval
(1.8.7).

Under strong feedback between treatment and confounders, the censoring
weights keep residual bias. Time-updated censoring covariates remove
part of the deviation selection bias, relative to covariates frozen at
baseline, and not all of it. Section 3 measures that residual bias.
Where feedback is central, g-methods are indicated: the parametric
g-formula, or g-estimation of structural nested models. This pipeline
does not implement them.

Unmeasured prognostic factors that drive adherence or loss violate
assumption 5, for example a healthy-adherer mechanism. They bias the
per-protocol estimand in any implementation, and the weight diagnostics
cannot detect them. The design addresses them, for example through
negative-control outcomes or sensitivity analyses for unmeasured
selection.

### 1.8 Data analysis

TARGET items 6h and 7h.i.

The study specification is machine-readable. It states the enrollments,
the outcomes, the horizons and the subgroups. For each estimand, the
results report weighted events, person-years and rates. They also report
the IRR with its interval and p-value, the risk difference and the
number needed to treat. The attrition counts persons and person-trials
separately.

The formulas below index persons by $i$, trials by $m$ and the follow-up
intervals of a trial by $j = 0,\ldots,K - 1$. Here $K$ is the horizon
divided by the width of the enrollment period, rounded up. $A_{i,m,0}$
is the assigned arm, 1 for the intervention. $L_{i,m,j}$ is the
confounder vector as most recently updated in interval $j$, and
$L_{i,m,0}$ is its value at the recruiting week. $Y_{i,m,j}$ is the
outcome indicator. $C_{i,m,j}^{L}$ and $C_{i,m,j}^{D}$ mark censoring by
loss and by deviation in interval $j$, and $D_{i,m,0}$ marks a deviation
at time zero (1.8.2).

$c_{m,j}$ is the calendar period of interval $j$. Calendar periods are
the blocks of calendar time that define the enrollment periods, so each
follow-up interval lies in exactly one. A trial with several follow-up
intervals therefore spans several calendar periods. Section 1.8.9 states
the three time axes, and the reason for the time terms of each model.

#### 1.8.1 Treatment weights

The baseline row of each person-trial is its first follow-up row, at
follow-up week 0. A logistic regression of the assigned arm on the
confounders at the recruiting week, as main effects, is fit there:

$${logit}\,{Pr}\left( A_{m,0} = 1 \mid L_{m,0} \right) = \gamma_{0} + \gamma^{\top}L_{m,0}.$$

The model reads the value at the recruiting week, because the value at
time zero would adjust for the wrong instant. One model covers all
trials of the enrollment, and it holds no term for the trial. The
stabilised weight uses the marginal fraction of intervention
person-trials as numerator:

$$SW^{A} = A_{m,0}\,\frac{\bar{p}}{\widehat{ps}} + \left( 1 - A_{m,0} \right)\,\frac{1 - \bar{p}}{1 - \widehat{ps}},\qquad\bar{p} = \widehat{Pr}\left( A_{m,0} = 1 \right),\qquad\widehat{ps} = \widehat{Pr}\left( A_{m,0} = 1 \mid L_{m,0} \right).$$

The weight is constant across the rows of a person-trial. The propensity
model holds main effects only. The protocol encodes strong non-linearity
or interactions as derived confounders.

#### 1.8.2 Censoring weights

Two causes censor per-protocol follow-up: loss of observation and
protocol deviation (1.4). $C_{m,j}^{L}$ marks the row that a loss stops,
and $C_{m,j}^{D}$ marks the row that a deviation stops. The row of an
outcome is never marked as censored (1.4). A deviation at time zero
stops follow-up before the first row, and $D_{m,0}$ marks it.

swereg fits one model for each cause, by default separately in each arm
$a$. Each model fits on the rows where its cause can be observed:

1.  Loss is observed at the end of a row, after the outcome of that row.
    The loss model fits on the rows without an outcome.
2.  A deviation is observed at the start of the next row, and a person
    who is lost cannot be seen to deviate. The deviation model fits on
    the rows without an outcome that were not lost.
3.  The time-zero model fits on every person-trial under follow-up at
    time zero, including those that deviate there and keep no row.
    `$s4_prepare_for_analysis()` keeps them in `$time_zero_deviation`.

Releases before 27.1.1 fitted one model for both causes, on every row
including the rows with an outcome. A row with an outcome cannot be
censored, so that model read it as evidence of staying uncensored.

The two row-level models are discrete-time complementary log-log
generalised additive models with a person-time offset. For cause $r$,
loss L or deviation D:

$${cloglog}\,{Pr}\left( C_{m,j}^{r} = 1 \mid {\text{at risk of}\mspace{6mu}}r,\ A_{m,0} = a \right) = s_{a}^{r}\left( u_{m,j} \right) + g_{a}^{r}\left( c_{m,j} \right) + \alpha_{a}^{r\top}L_{m,j} + \log\Delta_{m,j}.$$

Here $u_{m,j}$ is the start of interval $j$ in weeks from time zero.
$\Delta_{m,j}$ is the width of the row in weeks, and $\eta_{m,j}^{r}$ is
the linear predictor without the offset. The uncensoring probability is
then
$q_{m,j}^{r} = \exp\{ - \exp\left( \eta_{m,j}^{r} \right)\Delta_{m,j}\}$.
One linear predictor therefore gives $q(4) = q(1)^{4}$.

That identity makes a four-week row and a one-week row comparable. It
matters because the last row is clipped at its exact stop, so it can be
narrower than a whole interval. A logit link carries no such identity.

Both time terms follow the term ladder of 1.8.9. The follow-up term
$s_{a}^{r}$ depends on the number of distinct interval starts in the
risk set of the cause in the arm:

- 10 or more: a penalised spline;
- 4 to 9: a natural cubic spline of 3 degrees of freedom;
- 2 or 3: a factor;
- 1: no term.

A penalised spline asks for 10 basis functions, so it needs 10 distinct
values. The calendar term $g_{a}^{r}$ follows the same ladder on the
number of distinct calendar periods in the same risk set. Both counts
exclude the rows of zero width, which hold no person-time.

The confounders carry their updated value in each interval. Time-varying
confounders, where the data hold them, therefore inform the loss and
deviation models. Section 1.8.7 states how a missing value is filled.

The time-zero model is a logistic regression on the confounders at the
recruiting week, as the treatment-weight model is (1.8.1):

$${logit}\,{Pr}\left( D_{m,0} = 1 \mid A_{m,0} = a,\ L_{m,0} \right) = \delta_{a} + \beta_{a}^{\top}L_{m,0}.$$

$p_{m,0}$ is its fitted probability, and ${\bar{p}}_{a}$ is the
proportion of the person-trials of arm $a$ that deviate at time zero.

A cause with no censoring in its risk set fits no model, and its factor
is exactly 1. `$ipcw_formulas` then records
`list(fitted = FALSE, reason = )` for that cause. A row-level risk set
with no uncensored row stops the run, and so does a model that cannot be
fit. An arm in which every person-trial deviates at time zero keeps no
row, so no row needs its time-zero factor. Its three models are not
fitted, and the run does not stop. swereg substitutes no marginal
censoring rate for a model it could not fit.

The stabilised weight for the row in interval $k$ is

$$SW_{m,k}^{C} = \frac{1 - {\bar{p}}_{a}}{1 - p_{m,0}}\prod\limits_{j = 0}^{k - 1}\frac{{\bar{q}}_{a}^{L}(j)}{q_{m,j}^{L}}\prod\limits_{j = 0}^{k - 1}\frac{{\bar{q}}_{a}^{D}(j)}{q_{m,j}^{D}}.$$

Here ${\bar{q}}_{a}^{r}(j)$ is the numerator of cause $r$. It is a
second fit on the same risk set, with the follow-up term $s_{a}^{r}$ and
without the calendar term or the confounders (1.8.9). The deviation
model conditions on not being lost, so $q_{m,j}^{L}\, q_{m,j}^{D}$ is
the probability of remaining uncensored over row $j$. The denominator of
$SW_{m,k}^{C}$ is therefore the joint probability of remaining
uncensored through the start of interval $k$. The panel keeps the three
factors as `ipcw_pp_time_zero`, `ipcw_pp_loss` and `ipcw_pp_deviation`,
and `ipcw_pp` is their product. Two rules govern the ratio.

- The product is lagged. It stops at interval $k - 1$, so the
  uncensoring probability of a row does not enter its own weight. A
  censored interval stays in the risk set. The empty product gives the
  first interval of every person-trial its time-zero factor alone. An
  inclusive product belongs to data that delete the censoring row, and
  the two conventions are not mixed.
- The numerators are marginal. Canonical stabilisation (Danaei et
  al. 2013) uses a numerator model conditional on baseline covariates,
  which then enter the outcome model too. Here the outcome model holds
  no confounders (1.8.4), so each row-level numerator carries the
  follow-up term alone, and the time-zero numerator is the proportion
  ${\bar{p}}_{a}$. This keeps the marginal estimand consistent. It
  stabilises less when baseline covariates strongly predict censoring.

#### 1.8.3 Truncation

The analysis weight is the product of the two weights for the
per-protocol estimand, and the treatment weight alone for the
intention-to-treat analogue:

$$W_{i,m,j} = SW_{i,m}^{A} \times SW_{i,m,j}^{C}\;\;\text{(per-protocol)},\qquad W_{i,m,j} = SW_{i,m}^{A}\;\;\text{(intention-to-treat)}.$$

Weights are truncated at the 1st and 99th percentiles of the pooled rows
of all person-trials. The intention-to-treat analysis truncates the
treatment weight. The per-protocol analysis truncates the product and
not its components, so extreme components can offset each other. Primary
analyses use truncated weights. The untruncated per-protocol result is
reported beside them as a sensitivity analysis (1.9).

Truncation trades bias for variance. Clipping the weight tails reduces
the variance of the estimator. It also under-corrects the confounding or
selection that the clipped weights carried, and that moves the estimate.
Under near-violations of treatment positivity, it moves the estimate
toward the null. In the simulations of Section 3.8, the bias of the
truncated per-protocol fit grew with how strongly a measured covariate
drove the loss.

The truncated weight is the primary analysis on simulation evidence,
which section 3 reports. The scenarios included heavy loss to follow-up
strongly driven by covariates. In no per-protocol scenario did the
truncated fit have the larger sampling spread, and in most it had the
lower root-mean-squared error. Its advantage was largest where the
untruncated censoring weights were unstable. In the remaining scenarios,
the untruncated fit had the lower root-mean-squared error because it was
less biased. Section 3.8 gives the counts.

#### 1.8.4 Outcome model

The IRR comes from a weighted quasi-Poisson marginal structural model on
the analysis data:

$$\log E\left\lbrack Y_{i,m,j} \right\rbrack = \beta_{0} + \beta_{1}A_{i,m,0} + h\left( u_{m,j} \right) + f(m) + \log\left( \text{person-weeks}_{i,m,j} \right).$$

Here $u_{m,j}$ is the start of interval $j$ in weeks from time zero, and
$m$ is the trial. The person-weeks are the width of the row after
clipping. Both time terms follow the term ladder of 1.8.9 without the
penalised spline. Each is a natural cubic spline of 3 degrees of freedom
with 4 or more distinct values in the analysis data, a factor with 2 or
3, and absent with 1.

The trial term $f$ adjusts for differences between trials, while one
treatment coefficient is shared across trials (Danaei et al. 2013;
Caniglia et al. 2023). Section 1.8.9 states why the model reads the
trial and the interval start, and not the calendar period of the row or
the stop of the row. No confounders enter the outcome model, so
$\exp\left( \beta_{1} \right)$ is the marginal IRR. The model is fit by
weighted quasi-Poisson regression with survey-linearised variance
(1.8.6).

With rare events in each interval, as is typical in register data, the
IRR approximates the hazard ratio of a proportional-hazards model
(Thompson 1977). The Poisson working model stays feasible on data with
millions of person-trial intervals, where weighted Cox regression would
not. The quasi-Poisson variance function allows for overdispersion,
including that from the weights. Each IRR comes with weighted event
counts, person-years at 52.25 weeks per year, and rates per 100,000
person-years.

#### 1.8.5 Absolute scale

The survival of arm $a$ is a weighted product-limit estimate on the
distinct stop times of the rows. Every stop is a whole number of weeks,
so the estimate moves on a weekly grid:

$${\widehat{S}}_{a}(t) = \prod\limits_{u \leq t}\{ 1 - {\widehat{h}}_{a}(u)\},\qquad{\widehat{h}}_{a}(u) = \frac{\sum W_{i,m,j}\, Y_{i,m,j}}{\sum W_{i,m,j}}.$$

Both sums run over the person-trials of arm $a$. The numerator holds the
events at the stop of their own row. The denominator holds the weight of
every row at risk at $u$, so the risk set spans the stop time. A row is
at risk at $u$ when $t_{\text{start}} < u \leq t_{\text{stop}}$.

$W_{i,m,j}$ is the analysis weight of 1.8.3. It is the truncated
treatment weight for the intention-to-treat analogue and the truncated
product weight for the per-protocol estimand. Covariates do not enter
the estimator, so the weights carry the whole adjustment, as in the IRR
model. A stop time at which an arm has nobody at risk leaves the
survival of that arm unchanged. The risk difference is
${\widehat{S}}_{0}(t) - {\widehat{S}}_{1}(t)$.

#### 1.8.6 Inference

Standard errors are survey-linearised (a Huber–White sandwich) and
clustered on the person, not the person-trial. That accounts for the
repeated person-trials and the repeated intervals of one person (Hernán
et al. 2008; Danaei et al. 2013; Su et al. 2024). The interval of the
IRR is a Wald interval on the log scale,
$\exp\left( {\widehat{\beta}}_{1} \pm z\,\widehat{se} \right)$. Here $z$
is the two-sided normal critical value at the study’s confidence level,
95% unless the specification sets another, so $z$ is 1.96 at 95%. Two
caveats apply:

- The variance treats the estimated weights, the single imputation and
  the carry-forward as fixed. For stabilised weights this is usually
  slightly conservative for the treatment coefficient, but it is not
  exact. A person-level bootstrap of the whole pipeline, with every
  model refitted, is the fuller alternative.
- In the simulations of section 3, coverage is near nominal where the
  assumptions of the estimand hold. It is slightly below nominal under
  confounding with independent loss. When an estimand ignores
  informative loss, coverage falls because of bias, not because of the
  variance estimator.

The interval of the risk difference is a percentile interval from a
person-level (cluster) bootstrap, with 500 replicates and a fixed seed.
Each replicate draws $N$ persons with replacement, and a drawn person
brings all of their person-trials. One resample serves both arms. A
person can be a comparator in an early trial and in the intervention arm
of a later one. The survival estimates of the two arms are then
correlated, and separate resamples per arm would ignore that covariance.

The weights keep their estimated values in every replicate: the models
of 1.8.1 and 1.8.2 are not refitted. The level of the interval is the
study’s confidence level, the same level as the interval of the IRR.

At a stop time where either arm has no weighted event up to and
including that time, the risk difference has no interval. Every
replicate that is not missing would then give that arm a risk of exactly
zero, and the percentiles would reflect the other arm alone. The point
estimate is still reported.

The interval of the number needed to treat is
$\left( - 1/{RD}_{lo}, - 1/{RD}_{hi} \right)$. It exists only when the
risk-difference interval strictly excludes zero, because
$\left. x\mapsto - 1/x \right.$ is undefined at zero. Otherwise the
number needed to treat has no interval. Altman (1998) reports such an
interval as running from an NNTH through infinity to an NNTB, and this
implementation does not. The sign of the point estimate of the risk
difference alone decides benefit or harm.

#### 1.8.7 Missing data

Each kind of missing value has one rule:

- A missing confounder value at the recruiting week is singly imputed.
  The default draws one hot-deck value from the observed values, under a
  fixed seed.
- A missing confounder value during follow-up is carried forward from
  the last observed value of the same person-trial. The carry-forward
  starts from the value at the recruiting week. A person-trial with no
  observed value after entry therefore carries its entry value, imputed
  or not, through follow-up. swereg reports the filled rows and
  person-trials per confounder and per enrollment.
- A missing treatment value during follow-up is discordant in both arms
  (1.4).
- A missing treatment value in the enrollment period drops that week
  from the arm classification (1.3).
- A missing subgroup value removes the row from the analyses of that
  subgroup (1.8.8).

A missing treatment value also interacts with the washouts. One washout
rule excludes a week when an earlier week in its window holds the
washed-out value. Under that rule, a missing week makes every later week
ineligible while the window holds it. Under a lifetime washout of that
kind, one missing week removes the person from every later trial. The
other washout rule excludes a week when an earlier observed week holds
another value, and it skips missing weeks.

A week without observation is not a missing value. It is a loss of
observation, and it stops follow-up under both estimands (1.4). Neither
the imputation nor the carry-forward propagates its uncertainty into the
variance (1.8.6).

#### 1.8.8 Heterogeneity and subgroups

The protocol names each subgroup as a categorical baseline variable. For
each subgroup, the pipeline fits rate ratios within each level. It also
runs a joint Wald test of the interaction between treatment and the
subgroup. Both run for both estimands. A level with no events returns no
estimate rather than an unstable one.

The subgroup IRRs and the effect-modification test hold the time terms
of the IRR model (1.8.9). A joint Wald test of the interaction between
treatment and a natural spline of the trial tests heterogeneity across
trials. The spline has 3 degrees of freedom, or one fewer than the
number of trials where that is smaller. swereg provides that test, and
the pipeline does not run it. A study that wants it states it in the
protocol.

#### 1.8.9 Time terms

Every time term of 1.8 reads one of three time axes. Each row of the
analysis data carries all three. This subsection names them by their
columns, because the reasons for each model’s terms depend on what each
column holds.

| Column                 | Symbol    | Meaning                                                                                                    |
|:-----------------------|:----------|:-----------------------------------------------------------------------------------------------------------|
| `period_id`            | $c_{m,j}$ | The calendar period of the row.                                                                            |
| `enrollment_period_id` | $m$       | The trial. It is the enrollment period, which ends at time zero, and it is constant within a person-trial. |
| `tstart`               | $u_{m,j}$ | The time since time zero at the start of the interval, in weeks.                                           |

Follow-up interval $j$ of trial $m$ is interval number $j + 1$. With an
enrollment period of $w$ weeks, the three axes obey one identity.

$$c_{m,j} = m + (j + 1),\qquad u_{m,j} = j\, w.$$

In column terms, `period_id` = `enrollment_period_id` + interval number,
and the interval number is `tstart` / $w$ + 1. Figure A shows the three
axes on a Lexis diagram.

![Figure A. Each line is one person-trial from its time zero, and the
black lines are one person in three successive trials. The dashed lines
are one calendar period, one trial and one time since time
zero.](tte-methods_files/figure-html/time-term-lexis-1.png)

Figure A. Each line is one person-trial from its time zero, and the
black lines are one person in three successive trials. The dashed lines
are one calendar period, one trial and one time since time zero.

The models of 1.8 hold the time terms below. Here flex($x$) is the term
ladder below, and $n$ is the number of trials.

| Model                                   | Time terms                                    | Other terms                                            | Offset           | Family                 | Engine                      |
|:----------------------------------------|:----------------------------------------------|:-------------------------------------------------------|:-----------------|:-----------------------|:----------------------------|
| Treatment weights (1.8.1)               | none                                          | confounders at the recruiting week                     | none             | binomial, logit link   | logistic regression         |
| Loss and deviation denominators (1.8.2) | flex(`tstart`) + flex(`period_id`)            | time-updated confounders                               | log person-weeks | binomial, cloglog link | penalised GAM, or GLM (1.9) |
| Loss and deviation numerators (1.8.2)   | flex(`tstart`)                                | none                                                   | log person-weeks | binomial, cloglog link | as the denominator          |
| Time-zero deviation (1.8.2)             | none                                          | confounders at the recruiting week                     | none             | binomial, logit link   | logistic regression         |
| IRR and subgroup IRR (1.8.4, 1.8.8)     | flex(`tstart`) + flex(`enrollment_period_id`) | arm                                                    | log person-weeks | quasi-Poisson          | survey-weighted GLM         |
| Effect modification (1.8.8)             | flex(`tstart`) + flex(`enrollment_period_id`) | arm × factor(subgroup)                                 | log person-weeks | quasi-Poisson          | survey-weighted GLM         |
| Heterogeneity across trials (1.8.8)     | flex(`tstart`)                                | arm × ns(`enrollment_period_id`, df = min(3, $n$ − 1)) | log person-weeks | quasi-Poisson          | survey-weighted GLM         |

The person-weeks of the offset are the width of the row after clipping
(1.8.4). The loss and deviation models each count the distinct values of
their own risk set. The time-zero numerator is the proportion of the arm
that deviates at time zero, and it needs no model.

![Figure B. Each panel is the Lexis diagram of Figure A. Coloured lines
are the two axes that the model holds, and grey lines are the axis it
leaves out.](tte-methods_files/figure-html/time-term-models-1.png)

Figure B. Each panel is the Lexis diagram of Figure A. Coloured lines
are the two axes that the model holds, and grey lines are the axis it
leaves out.

##### At most two axes in each model

The outcome models and the loss and deviation denominators each hold two
of the three axes. Their numerators hold `tstart` only. The
treatment-weight model and the time-zero model hold no time term. The
identity makes the third axis a linear function of the other two. This
is the age–period–cohort identity of Lexis-diagram analysis. A linear
term in the third axis therefore adds nothing to a model that holds the
other two. Only the curvature of the third axis can add to the fit, and
the data identify that curvature weakly.

A grid of 10 trials and 8 follow-up intervals shows this. Natural cubic
splines of 3 degrees of freedom in `enrollment_period_id` and `tstart`,
with an intercept, give a design matrix of rank 7. A linear `period_id`
term leaves the rank at 7. A natural cubic spline of 3 degrees of
freedom in `period_id` raises it to 9.

##### Outcome models: the trial and the time since time zero

The outcome models hold the trial and the time since time zero, as the
sequential trial literature does. Danaei et al. (2013, p. 76) pooled 83
trials into one model. That model included “‘month at the trial’s
baseline’ (taking values from 1 to 83) and month of follow-up in each
‘trial’”. Each entered as a continuous covariate with its squared term.
To test heterogeneity, they added “a product term between the indicator
for therapy initiation and the month of the ‘trial’” (p. 76). The
heterogeneity test of 1.8.8 is that test, with a natural spline of the
trial in place of the linear product term.

Caniglia et al. (2023, author manuscript p. 5) pooled 13 trials and
included “‘trial’ (taking values from 1-13 and modeled with restricted
cubic splines)”. Their outcome model is a log-binomial model of a risk
at delivery, so it holds no follow-up time. TrialEmulation (Su et
al. 2024, p. 17) adds `trial_period` and `followup_time` to the outcome
model by default, each as a linear and a quadratic term. There,
`trial_period` is the trial index and `followup_time` is “the follow-up
visit number within trials” (p. 16).

The trial term adjusts for differences in the event rate between trials.
One treatment coefficient is then shared by every trial.

##### Censoring denominators: the calendar period and the time since time zero

The loss and deviation denominators hold the calendar period of the row
and the time since time zero. Deviation and loss respond to the time on
the protocol, for example a stop early in treatment. They also respond
to calendar time, for example a change in prescribing practice. Such a
change acts on every open trial in the same calendar period, so the
calendar period is its axis.

Danaei et al. (2013, p. 79) used inverse probability weights against the
selection that the artificial censoring causes. Their model for
treatment use included “calendar month of follow-up and its squared
term”. The same model also included the baseline calendar year.

##### Censoring numerators: the time since time zero only

The loss and deviation numerator models hold the time since time zero
and nothing else. Su et al. (2024, p. 6–7, the text after equation 3)
state the condition. Covariates “conditioned upon in the numerator terms
of the stabilised weights must be included as covariates in the marginal
structural model”. The outcome model holds the trial and the time since
time zero. A spline or factor in `period_id` can fall outside the span
of those two terms, as the rank example above shows. The numerator
therefore omits it.

By default the numerator fits in each arm, so it also conditions on the
arm, which the outcome model holds. A numerator that depends only on
terms of the outcome model leaves the estimand unchanged. The cost of
the smaller numerator is therefore more variable weights, and not bias.

##### Time read at the start of the interval

Every model with a time-since-entry term reads it at the interval start,
`tstart`, and not at the interval stop, `tstop`. The last interval of a
person-trial is cut short at an event or a censoring, so its `tstop` is
smaller than a whole interval. A flexible term in `tstop` could then fit
those last rows apart from the other rows, and the last rows hold the
events. Under `tstart`, every row of interval number $k$ sits at
$(k - 1)\, w$ weeks.

Danaei et al. (2013, p. 76) index follow-up by month of follow-up.
TrialEmulation indexes it by `followup_time`, the visit number (Su et
al. 2024, p. 16). Both are interval numbers. The clipped width still
enters the offset through the person-weeks. Figure C shows two
person-trials under both readings.

![Figure C. Bars are the rows of two person-trials, and the red row ends
at an event in week 10. Dots place each row at tstart, and crosses place
it at tstop.](tte-methods_files/figure-html/time-term-intervals-1.png)

Figure C. Bars are the rows of two person-trials, and the red row ends
at an event in week 10. Dots place each row at tstart, and crosses place
it at tstop.

##### The term ladder

flex($x$) depends on the number of distinct values of $x$ in the rows
the model fits.

- 10 or more, in a loss or deviation model fit as a penalised GAM: a
  penalised spline. It asks for 10 basis functions, mgcv’s default, so
  it needs 10 distinct values.
- 4 or more otherwise: a natural cubic spline of 3 degrees of freedom.
  With the intercept it has 4 parameters, so it needs 4 distinct values.
- 2 or 3: a factor, which needs 2.
- 1: no term.

The penalised spline fits only inside the GAM engine. Every outcome
model and the GLM loss and deviation models of 1.9 therefore start the
ladder at the natural spline. A loss or deviation model counts the
distinct values in its own risk set. It counts them in each stratum, by
default each arm, after the rows of zero width leave. An outcome model
counts them in the rows it fits, so a subgroup IRR counts the rows of
its subgroup level.

##### Column names and stored objects

Earlier releases named the calendar-period column `trial_id`. On a
follow-up row that column held the calendar period of the row, and not
the trial. The name hid that the outcome model read the calendar period.
swereg now names each column by what it holds: `period_id` for the
calendar period of a row, and `enrollment_period_id` for the trial.

A saved R6 object runs the method bodies it was saved with. A stored
enrollment or plan from an earlier release would fit the earlier time
terms with no warning. swereg therefore refuses to read a stored
enrollment below schema 6 and a stored plan below schema 5. Rebuild the
plan with s0, then run s1, s2 and s3 again.

### 1.9 Sensitivity analyses

TARGET item 7h.ii.

The pipeline reports one sensitivity analysis by default: the
per-protocol IRR with untruncated weights. It is a required companion of
the primary analysis, not an alternative primary analysis. A material
divergence between the two estimates means the weights are under stress.
It calls for two checks: the raw weight distribution, and the positivity
of treatment and censoring. Where the extreme weights are structural,
restrict the eligible population rather than truncate harder.

The loss and deviation models can also be refit as generalised linear
models, which drops the penalised splines. Each time term of 1.8.2 is
then a natural cubic spline of 3 degrees of freedom with 4 or more
distinct values. It is a factor with 2 or 3, and absent with 1. Each
numerator keeps the follow-up term alone (1.8.9). The time-zero model is
a logistic regression under both settings.

### 1.10 Limitations

TARGET item 16 and RECORD item 19.1 (Benchimol et al. 2015).

- No grace periods and no cloning. Deviation censors at the start of the
  first discordant week beyond the tolerance of the arm (1.4).
- The estimand conditions on reaching time zero. It says nothing about
  people who die, or have an outcome, before time zero (1.6).
- Trials open once per enrollment period, so a person who becomes
  eligible inside an enrollment period waits for the next time zero.
- The censoring models carry no lagged treatment term, so they hold no
  adherence history.
- There is no as-treated estimand.
- The absolute risk is cause-specific. Death censors follow-up, and no
  competing-risk cumulative incidence is estimated (1.6).
- The bootstrap for the risk difference holds the weights fixed, so its
  interval leaves out the uncertainty of the weight models (1.8.6).
- The single imputation and the carry-forward do not propagate their
  uncertainty into the variance (1.8.7).
- Comparator sampling discards comparator information, which costs
  precision (1.3).
- The propensity model and the time-zero model hold main effects only.
  The loss and deviation models hold main effects and time terms, which
  are smooth only with enough distinct values (1.8.2). Non-linearities
  need derived variables.

Each study adds its own limitations, such as misclassification and
unmeasured confounding in the registers.

------------------------------------------------------------------------

## 2. Manuscript methods

Replace each placeholder in the text below with the value of the study.

| Placeholder      | Value                                                                        |
|------------------|------------------------------------------------------------------------------|
| `[intervention]` | the intervention treatment                                                   |
| `[comparator]`   | the comparator strategy                                                      |
| `[outcome]`      | the outcome                                                                  |
| `[confounders]`  | the confounders                                                              |
| `[width]`        | the width of the enrollment period, in weeks                                 |
| `[washout]`      | the new-user washout window                                                  |
| `[k]`            | the number of comparators drawn per intervention person-trial, for example 2 |
| `[horizon]`      | the analysis horizon                                                         |
| `[end]`          | the administrative end of study                                              |
| `[tolerance]`    | the tolerance of each arm, in weeks                                          |
| `[subgroups]`    | the pre-specified subgroups                                                  |

We emulated a sequence of target trials of `[intervention]` versus
`[comparator]` for `[outcome]` (Hernán et al. 2008; Hernán and Robins
2016; Cashin et al. 2025). The data came from the Swedish national
health registers.

### Eligibility, treatment strategies and assignment

Eligible individuals could start treatment at many calendar times. We
therefore emulated a sequence of trials rather than a single trial
(Hernán et al. 2008; Danaei et al. 2013; Caniglia et al. 2023). A new
trial opened every `[width]` weeks, and its enrollment period lasted
`[width]` weeks. We assessed every eligibility criterion in every week.
Look-back windows counted calendar weeks, not data rows. Each enrollment
could add its own criteria to the global criteria, including a range of
calendar years for trial entry.

A new-user criterion excluded any week with use of `[intervention]` in
the `[washout]` before it. With a lifetime washout, each person started
treatment in at most one trial. With a finite washout, a person could
start again in a later trial after `[washout]` without treatment.

Assignment used the weeks of the enrollment period in which the
individual was eligible and held one of the two arm values. Individuals
entered the intervention arm if at least one of those weeks held
`[intervention]`. They entered the comparator arm if all of those weeks
held `[comparator]`. Individuals with no such week were ineligible for
that trial.

All intervention person-trials were enrolled. To bound computation, we
drew comparators by `[k]`:1 incidence density sampling within each
trial, with a seed stated in the specification. The draw took `[k]`
times the number of intervention person-trials of that trial, or every
remaining comparator where fewer remained. It was stratified by the
enrollment period and read no other variable. It formed no matched sets,
and no later step conditioned on one.

### Follow-up and outcomes

Time zero was a landmark: the first week after the enrollment period
closed. A person entered the trial only if they reached that week under
observation and free of every enrollment outcome. The enrollment period
therefore contributed no follow-up and no immortal time (Hernán and
Robins 2016; Caniglia et al. 2023).

Follow-up ended at the earliest of `[outcome]`, loss of observation, the
administrative end of study at `[end]` and the horizon of `[horizon]`.
Loss of observation was the first week after time zero without
observation, whether a gap or the end of the record. No tolerance
applied to it, and an outcome in that week or later was not counted.
Every stop was exact to the week. The risk was cause-specific: death and
the end of observation censored follow-up, and we did not model them as
competing risks.

Per-protocol follow-up also ended at protocol deviation: a run of
consecutive weeks off the assigned strategy longer than `[tolerance]`
weeks. A missing treatment status counted as a week off the strategy.
Follow-up ended at the start of the first week beyond the tolerance. An
`[outcome]` event in that week or later was not counted (Danaei et
al. 2013). A deviation did not end intention-to-treat follow-up.

### Causal contrasts and identifying assumptions

We estimated two estimands (Danaei et al. 2013). Both conditioned on
reaching the time zero of each trial under observation and free of the
study outcomes. The observational analogue of the intention-to-treat
effect compared the arms as assigned, ignoring later changes in
treatment. The per-protocol effect compared sustained `[intervention]`
with sustained `[comparator]`.

We reported both estimands as marginal incidence rate ratios (IRRs),
with weighted event counts and rates per 100,000 person-years by arm. We
also reported the risk difference at the end of follow-up. Beside it, we
reported the number needed to treat for benefit (NNTB) or for harm
(NNTH) (Altman 1998).

Identification relied on consistency, positivity, and no unmeasured
confounding of assignment given `[confounders]`. The per-protocol effect
also relied on the censoring models capturing the common causes of
deviation, loss and the outcome.

### Statistical analysis

We adjusted for `[confounders]` by stabilised inverse probability of
treatment weights from a logistic model (Hernán et al. 2008). The model
read each confounder at the recruiting week, the first eligible week of
the enrollment period with an arm value. For the per-protocol effect, we
added stabilised inverse probability of censoring weights, fitted
separately by arm. The weight was the product of three factors, one for
each way that per-protocol follow-up could be censored. A discrete-time
model for loss of observation was fitted on the intervals without an
outcome. A discrete-time model for protocol deviation was fitted on the
intervals without an outcome that were not lost. Both models included
the most recent values of `[confounders]`, the time since time zero and
the calendar period of follow-up (Danaei et al. 2013). Each time term
was a penalised spline where enough distinct values allowed one, and
simpler otherwise. Their numerator models included the time since time
zero only. A logistic model of `[confounders]` at the recruiting week
gave the probability of a deviation at time zero. It was fitted on every
person-trial, including those that the deviation left with no follow-up.
Its numerator was the proportion that deviated at time zero.

We truncated the weights at the 1st and 99th percentiles (Danaei et al.
2013). We fitted a weighted quasi-Poisson marginal structural model of
the event indicator on the assigned arm, with log person-time as the
offset. It included terms for the trial and for the time since time zero
at the start of each interval (Danaei et al. 2013; Su et al. 2024). Each
was a natural spline of 3 degrees of freedom with 4 or more distinct
values, and a factor with 2 or 3. The exponentiated coefficient of the
arm estimated the marginal IRR, pooled across trials.

With rare events, the IRR approximated the marginal hazard ratio
(Thompson 1977). Individuals contributed several observations within and
across trials. Confidence intervals therefore used survey-linearised
(sandwich) standard errors clustered on the person (Hernán et al. 2008;
Danaei et al. 2013).

For the absolute scale, we estimated the survival of each arm with a
weighted product-limit estimator on the weekly grid of stop times. It
used the same weights as the IRR. The risk difference was the difference
between the arms in one minus survival. Its confidence interval, at the
pre-specified level, was the percentile interval of 500 bootstrap
replicates. Each replicate resampled persons, not person-trials, and one
resample served both arms. The weights were held fixed in the bootstrap.

No interval was reported at a time by which either arm had no weighted
event. The number needed to treat was the negative reciprocal of the
risk difference, so a positive value meant benefit. We reported its
interval only when the interval of the risk difference excluded zero.

A missing confounder value at the recruiting week was singly imputed by
a hot-deck draw. A missing value during follow-up was carried forward
within the person-trial. We assessed effect modification by
`[subgroups]` with Wald tests of the interaction between treatment and
subgroup. We also estimated the IRR within each subgroup level.

### Sensitivity analyses

We repeated the per-protocol analysis with untruncated weights, as a
pre-specified sensitivity analysis. We read a divergence between the two
estimates as a sign of unstable weights.

### Software

Analyses used R with the swereg package, which provided the sequential
enrollment, weighting and estimation described above. The loss and
deviation models were fitted with mgcv, and the weighted outcome
regression with
[`survey::svyglm()`](https://rdrr.io/pkg/survey/man/svyglm.html). The
implementation was validated against simulated data with known true
effects, and against the TrialEmulation package (Su et al. 2024). The
validation suite of the package ran in continuous integration.

------------------------------------------------------------------------

## 3. Validation evidence

Every estimate, bias, coverage and limit in this section is computed
from a results artifact. None is transcribed by hand. The fixed
simulation design values are stated as inputs, for example the loss
hazard of 0.06 and ${expit}\left( - 2.4 + 0.9\, L_{0} \right)$. The
artifact comes from a rerun of the complete validation battery. The
rerun uses the same data-generating processes, truth calculations and
fit wrappers that the package’s test suite enforces in continuous
integration. Section 4.2 maps each layer to its test file and describes
how to regenerate the artifact.

Provenance: generated 2026-10-09 04:45:09 UTC with swereg 27.2.0,
TrialEmulation 0.0.4.11, under R version 4.5.2 (2025-10-31).

### 3.1 Design of the validation battery

The battery follows one principle. An estimator is validated when it
recovers a truth that is *known by construction*, not when it agrees
with another implementation. Agreement between two packages is
corroborating evidence only. Two correct implementations of the same
estimand must agree, but two implementations can also agree while both
miss the truth. The battery includes a scenario that shows this:
informative loss under the ITT estimand, where both packages share one
design bias (3.3).

The battery uses three kinds of truth:

- **Exact truths** for scenarios s1 to s4 (3.3, 3.6 and 3.9). Section
  3.9 defines them. The log-IRR truth is the limit of swereg’s weighted
  outcome model, with each arm and period weighted by its probability of
  remaining uncensored.

- **A Monte Carlo truth** for the stress and grid cells (3.4 and 3.8).
  It simulates 200,000 persons per arm under the forced strategy,
  without loss, and takes the log ratio of first-event incidence rates.
  The per-protocol truth holds treatment at the assigned value in every
  period. The ITT truth forces the baseline value only.

- **A planted truth** for the plan-layer cells (3.5), which Section 3.5
  states.

The Monte Carlo truth ignores censoring. Where the hazard ratio changes
over follow-up and loss removes later periods, it differs from the exact
truth. A bias against it then includes that difference, which Section
3.9 measures.

Five layers separate concerns, so that a failure localises to a pipeline
segment:

| Layer                         | Pipeline segment exercised                                                                                                                                 | Question answered                                                                                                                                                          |
|:------------------------------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------|:---------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Cross-package matrix (3.3)    | Enrollment-layer estimators (IPW, IPCW, weighted MSM) on person-period panels                                                                              | Do swereg and TrialEmulation each recover known truth where the estimand’s assumptions hold, and fail identically where they do not?                                       |
| Stress matrix (3.4)           | The same estimators at design extremes                                                                                                                     | Does the estimator remain stable under rare outcomes, null and harmful effects, near-positivity violation, heavy informative attrition, and treatment-confounder feedback? |
| Plan-layer truth matrix (3.5) | The complete production pipeline: specification, enrollment periods, sequential eligibility, the comparator draw, worker subprocesses, dual analysis files | Does the pipeline as a whole recover a planted constant-hazard truth, including the separation of PP from ITT under discontinuation?                                       |
| Coverage calibration (3.6)    | The sandwich variance estimator                                                                                                                            | Do nominal 95% intervals cover the truth 95% of the time when the estimand is valid?                                                                                       |
| Exact-truth cells (3.9)       | The enrollment-layer estimators, on the risk-difference scale as well as the log-IRR scale                                                                 | Is the risk difference at each horizon unbiased against an exact truth, and does its bootstrap interval cover it?                                                          |

Table 1. The five layers of the validation battery.

Interpreting single-dataset cells requires one calibration. At the
sample sizes used here, one simulated dataset carries Monte Carlo noise
(standard deviation) of 0.02 to 0.04 on the log-IRR scale. A single-run
gap of that order is therefore indistinguishable from zero. All cells
run at fixed seeds and are exactly reproducible. The multi-replicate
cells (Tables 5, 8, 13–17, 18 and 19) quantify bias and coverage across
repeated draws, free of this caveat.

### 3.2 Enrollment-layer scenarios: data-generating processes

The enrollment-layer cells (3.3, 3.4) share one person-period
data-generating process. For person $i$ with standard-normal baseline
confounder $L_{0i}$ and periods $t = 0,\ldots,19$:

$${logit}\,{Pr}\left( A_{i0} = 1 \right) = - 0.3 + \phi_{A}L_{0i}\qquad\text{(baseline initiation)}$$

$${logit}\,{Pr}\left( A_{it} = 1 \right) = - 3.0 + \phi_{S}L_{0i} + 8\, A_{i,t - 1},\quad t \geq 1\qquad\text{(switching with persistence)}$$

$${logit}\,{Pr}\left( Y_{it} = 1 \right) = - 3.5 + \theta A_{it} + \phi_{Y}L_{0i}\qquad\text{(outcome)}$$

with true contemporaneous treatment effect $\theta = - 0.7$ unless a
cell varies it. The persistence coefficient of 8 keeps most initiators
on treatment (adherent person-time dominates) while still generating
enough switching to separate the PP and ITT truths. Loss to follow-up,
when present, is geometric dropout from a per-person hazard. Independent
loss has a constant hazard of 0.06 per period. Informative loss has the
hazard ${expit}\left( - 2.4 + 0.9\, L_{0i} \right)$, so dropout selects
on the confounder that also drives treatment and outcome. The three
standard scenarios switch the nuisance parameters only, leaving the true
effect identical:

| Scenario | $\phi_{A}$ | $\phi_{S}$ | $\phi_{Y}$ |         Loss to follow-up          | What it induces                                                    |
|:---------|:----------:|:----------:|:----------:|:----------------------------------:|:-------------------------------------------------------------------|
| s1       |     0      |     0      |     0      |                none                | clean benchmark: no confounding, no selection                      |
| s2       |    0.6     |    0.4     |    0.4     |     independent (hazard 0.06)      | baseline confounding plus outcome-independent attrition            |
| s3       |    0.6     |    0.4     |    0.4     | informative (expit(-2.4 + 0.9 L0)) | baseline confounding plus attrition that selects on the confounder |

Table 2. Nuisance configuration of the three standard scenarios.

Each scenario dataset is simulated at 20,000 persons and 20 periods with
a fixed seed. Table 3 reports the realized characteristics of the exact
datasets analysed in 3.3.

| Scenario | Persons | Person-periods | Person-periods lost | Initiators at baseline | Persons with ≥1 event | Event risk per period |
|:---------|--------:|---------------:|--------------------:|-----------------------:|----------------------:|----------------------:|
| s1       |  20,000 |        400,000 |                  0% |                  43.0% |                 6,692 |                 2.05% |
| s2       |  20,000 |        236,519 |                 41% |                  44.0% |                 4,300 |                 2.18% |
| s3       |  20,000 |        197,823 |                 51% |                  44.0% |                 3,328 |                 1.93% |

Table 3. Realized descriptives of the three scenario datasets.

### 3.3 Cross-package validation matrix

Each scenario dataset is fed through the full triangle (known
potential-outcome truth, swereg, and `TrialEmulation`) for both
estimands. Estimates are compared on a common rate-ratio scale.
`TrialEmulation` reports odds ratios from pooled logistic regression.
The Zhang–Yu relation converts them with the reference-arm per-period
event risk of the Monte Carlo truth simulation (Zhang and Yu 1998;
Section 3.7). The truth is the exact log-IRR of Section 3.9.
`TrialEmulation` is a peer required to recover the truth itself, not an
oracle.

| Scenario | Nuisances                      | Estimand | True log-IRR |         swereg \[95% CI\] | TrialEmulation \[95% CI\] | swereg bias | TE bias | swereg − TE |
|:---------|:-------------------------------|:---------|-------------:|--------------------------:|--------------------------:|------------:|--------:|------------:|
| s1       | none                           | pp       |       -0.685 | -0.708 \[-0.763, -0.654\] | -0.708 \[-0.764, -0.653\] |      -0.023 |  -0.023 |      +0.000 |
| s1       | none                           | itt      |       -0.467 | -0.499 \[-0.549, -0.449\] | -0.499 \[-0.549, -0.449\] |      -0.032 |  -0.032 |      -0.000 |
| s2       | confounding + independent loss | pp       |       -0.667 | -0.639 \[-0.707, -0.570\] | -0.662 \[-0.731, -0.593\] |      +0.028 |  +0.005 |      +0.023 |
| s2       | confounding + independent loss | itt      |       -0.486 | -0.474 \[-0.538, -0.410\] | -0.480 \[-0.544, -0.417\] |      +0.012 |  +0.006 |      +0.006 |
| s3       | confounding + informative loss | pp       |       -0.668 | -0.618 \[-0.702, -0.535\] | -0.680 \[-0.759, -0.601\] |      +0.050 |  -0.012 |      +0.061 |
| s3       | confounding + informative loss | itt      |       -0.502 | -0.537 \[-0.612, -0.461\] | -0.544 \[-0.619, -0.469\] |      -0.035 |  -0.042 |      +0.007 |

Table 4. Cross-package validation matrix (N = 20,000, T = 20 periods,
fixed seed), against the exact log-IRR truth. swereg estimates use the
primary truncated weights. Log-IRR scale.

In Table 4, every interval of both packages covers the exact truth. That
includes the s3 per-protocol cell, whose censoring weights (1.8.2) model
the informative loss. In the s3 ITT cell both estimates are below the
truth (swereg -0.035, TrialEmulation -0.042), and they differ from each
other by 0.007. A single dataset nonetheless provides limited evidence.
At N = 20,000 one estimate carries Monte Carlo noise of 0.02 to 0.04 on
the log-IRR scale. Point estimates therefore deviate visibly from the
truth even under an unbiased estimator, and sampling variation alone
sets the size of any one gap. Replication gives a stronger assessment.
The full triangle is repeated on 20 independent datasets per scenario.
That reduces the Monte Carlo standard error of the estimated bias by a
factor of $\sqrt{20}$.

| Scenario | Estimand | Datasets | True log-IRR | swereg, truncated weights: mean bias (MC SE) | swereg, untruncated weights: mean bias (MC SE) | TrialEmulation: mean bias (MC SE) | Mean \|swereg − TE\| |
|:---------|:---------|---------:|-------------:|---------------------------------------------:|-----------------------------------------------:|----------------------------------:|---------------------:|
| s1       | pp       |       20 |       -0.685 |                               -0.004 (0.006) |                                 -0.004 (0.006) |                    -0.004 (0.006) |                0.000 |
| s1       | itt      |       20 |       -0.467 |                               -0.003 (0.005) |                                 -0.003 (0.005) |                    -0.003 (0.005) |                0.000 |
| s2       | pp       |       20 |       -0.667 |                               +0.020 (0.010) |                                 +0.007 (0.010) |                    -0.010 (0.010) |                0.030 |
| s2       | itt      |       20 |       -0.486 |                               +0.014 (0.009) |                                 +0.006 (0.009) |                    -0.001 (0.009) |                0.015 |
| s3       | pp       |       20 |       -0.668 |                               +0.044 (0.010) |                                 +0.037 (0.015) |                    -0.009 (0.010) |                0.054 |
| s3       | itt      |       20 |       -0.502 |                               -0.017 (0.009) |                                 -0.024 (0.009) |                    -0.029 (0.010) |                0.013 |

Table 5. Replicated cross-package matrix: mean bias against the exact
log-IRR truth over 20 independent datasets per scenario (N = 20,000
each), with the Monte Carlo standard error of the mean. swereg is shown
with its primary (1st/99th percentile) weight truncation and with
untruncated weights. Log-IRR scale.

![Figure 1. Bias of the estimated log-IRR against the exact truth over
20 independent datasets per scenario (N = 20,000 each) in the scenarios
whose assumptions every fit satisfies: s1 (no confounding, no loss) and
s2 (confounding, outcome-independent loss). Faint points are individual
datasets. Solid points are the mean bias with its 95% Monte Carlo
interval, and the vertical line marks zero bias. Every fit is within 3.5
Monte Carlo standard errors of zero, for both estimands and in both
scenarios (largest \|z\| 2.1). Figure 2 shows the informative-loss
scenarios.](tte-methods_files/figure-html/unnamed-chunk-12-1.png)

Figure 1. Bias of the estimated log-IRR against the exact truth over 20
independent datasets per scenario (N = 20,000 each) in the scenarios
whose assumptions every fit satisfies: s1 (no confounding, no loss) and
s2 (confounding, outcome-independent loss). Faint points are individual
datasets. Solid points are the mean bias with its 95% Monte Carlo
interval, and the vertical line marks zero bias. Every fit is within 3.5
Monte Carlo standard errors of zero, for both estimands and in both
scenarios (largest \|z\| 2.1). Figure 2 shows the informative-loss
scenarios.

![Figure 2. Bias under informative loss to follow-up, the scenarios in
which the estimators differ materially: s3 and its one-parameter
variants (designs in Table 16, Section 3.8). s3 has 20 datasets, against
the exact truth. Each variant has 10, against the Monte Carlo truth. Top
panel: for the intention-to-treat estimand the informative loss violates
the estimand's own assumptions, and every fit is below the truth; no
estimation method corrects an invalid estimand. Bottom panel: for the
per-protocol estimand the fits differ by how they correct the selection.
TrialEmulation conditions on the baseline covariate that drives the
loss, which is exact in the measured-driver designs. swereg corrects the
selection by censoring weights. No fit has the smallest bias in every
cell (Section
3.8).](tte-methods_files/figure-html/unnamed-chunk-14-1.png)

Figure 2. Bias under informative loss to follow-up, the scenarios in
which the estimators differ materially: s3 and its one-parameter
variants (designs in Table 16, Section 3.8). s3 has 20 datasets, against
the exact truth. Each variant has 10, against the Monte Carlo truth. Top
panel: for the intention-to-treat estimand the informative loss violates
the estimand’s own assumptions, and every fit is below the truth; no
estimation method corrects an invalid estimand. Bottom panel: for the
per-protocol estimand the fits differ by how they correct the selection.
TrialEmulation conditions on the baseline covariate that drives the
loss, which is exact in the measured-driver designs. swereg corrects the
selection by censoring weights. No fit has the smallest bias in every
cell (Section 3.8).

Averaging over 20 datasets reduces the Monte Carlo standard error of the
estimated bias to at most 0.015. That resolves systematic effects that
no single dataset can. Table 5 and Figures 1 and 2 show three results.

In s1 and s2, every fit is within 3.5 Monte Carlo standard errors of the
exact truth, for both estimands (Figure 1). This holds for swereg with
truncated and with untruncated weights, and for `TrialEmulation`.

In the s3 per-protocol cell, the untruncated swereg fit, +0.037 (MC SE
0.015), and `TrialEmulation`, -0.009 (MC SE 0.010), are within 3.5 Monte
Carlo standard errors of the truth. The truncated swereg fit is not: its
mean bias is +0.044 (MC SE 0.010). The difference, 0.008, is the
bias–variance tradeoff described in 1.8.3. Informative dropout means
that the high-risk individuals still under observation late in follow-up
carry large censoring weights, to represent those who left. The
1st/99th-percentile truncation caps exactly those weights, and the
under-corrected selection shows as bias toward the null. For this reason
the untruncated per-protocol results are exported as a sensitivity
analysis. A material divergence between the truncated and untruncated
estimates shows that truncation attenuates the correction. Section 3.9
shows the same shift on the risk-difference scale in s2 and s4.

The s3 ITT cell is of a different kind. Every fit is below the truth:
swereg -0.017 with truncated and -0.024 with untruncated weights, and
`TrialEmulation` -0.029. The ITT analysis carries no loss weight and
assumes loss independent of the outcome (assumption 4 of 1.7).
Informative loss therefore biases it in any correct implementation. The
exact limit of an ITT fit without loss weights is -0.027 from the truth.
Each of the three means is within 3.5 Monte Carlo standard errors of
that limit. Cross-package agreement is therefore not evidence of
correctness, which is why every layer of this battery is anchored to a
known truth.

### 3.4 Stress matrix

The stress cells reuse the Section 3.2 data-generating process with one
or two parameters pushed to an extreme. Each cell probes a specific
failure mode. Table 6 specifies the designs; the cells then follow in
order.

| Cell                          | Design deviation from the base DGP                                                                              | What it probes                                                            |
|:------------------------------|:----------------------------------------------------------------------------------------------------------------|:--------------------------------------------------------------------------|
| Rare outcome                  | Outcome intercept −6.0 (0.18% risk/period), N = 40,000, θ = −0.7                                                | Sparse-event stability of the weighted MSM and the spline IPCW model      |
| Null effect                   | θ = 0, independent loss, N = 20,000                                                                             | False-positive effects (does the pipeline manufacture signal from noise?) |
| Informative attrition         | Dropout hazard expit(−1.3 + 0.9 L0): 73% of person-periods lost, selecting on the confounder; N = 30,000        | IPCW under heavy selection; the ITT arm of this cell is expected to fail  |
| Harmful effect, depletion     | θ = +0.7, three independent seeds at N = 20,000, TrialEmulation cross-check                                     | The person-time-weighted-average interpretation of the pooled IRR (1.6)   |
| Near-positivity violation     | φA = 1.5: propensity scores span 0–1; ITT fit at three truncation levels                                        | The truncation bias-variance tradeoff (1.8.3)                             |
| Treatment-confounder feedback | AR(1) confounder Lt = 0.7 Lt−1 − 0.4 At−1 + εt driving both switching (0.8 Lt) and outcome (0.5 Lt); N = 25,000 | The residual bias of the censoring weights under feedback (1.7)           |
| Determinism                   | Identical data, PP estimator fit twice                                                                          | Uncontrolled stochastic steps anywhere in the fit                         |

Table 6. Stress-cell designs. All other parameters as in Section 3.2; T
= 20 periods, fixed seeds.

| Cell                  | Estimand | True log-IRR |       Estimate \[95% CI\] |   Bias | CI covers truth | Note                                |
|:----------------------|:---------|-------------:|--------------------------:|-------:|:---------------:|:------------------------------------|
| rare_outcome          | pp       |       -0.696 | -0.655 \[-0.783, -0.528\] | +0.041 |       yes       | event risk 0.18%/follow-up interval |
| rare_outcome          | itt      |       -0.435 | -0.421 \[-0.534, -0.308\] | +0.014 |       yes       |                                     |
| null_effect           | itt      |        0.000 |   0.041 \[-0.012, 0.094\] | +0.041 |       yes       | true log-IRR = 0                    |
| informative_attrition | pp       |       -0.659 | -0.695 \[-0.787, -0.602\] | -0.035 |       yes       | 73% of person-periods lost          |
| informative_attrition | itt      |       -0.444 | -0.573 \[-0.656, -0.490\] | -0.129 |       no        | biased by design: no loss weight    |

Table 7. Stress cells, single dataset at fixed seed; swereg estimates
use the primary truncated weights. Log-IRR scale.

Three observations from Table 7. At an event risk of 0.18% per period,
the per-protocol fit completes, including its spline-based censoring
models. Its interval covers the truth (bias +0.041). Under a true null,
the interval covers zero: the weighting and pooling machinery does not
make an effect from noise. In the attrition cell, confounder-driven
dropout removes 73% of the person-periods. There, the per-protocol
interval covers the truth (bias -0.035). The ITT estimator carries no
loss weight by construction. It is displaced (bias -0.129), and its
interval excludes the truth. This is the designed failure that motivates
the estimand distinction in practice.

Determinism: refitting the per-protocol estimator on identical data
reproduced the estimate to a maximum absolute difference of 0; the
pipeline has no uncontrolled stochastic step.

| Seed | True log-IRR (cumulative-rate) | swereg | TrialEmulation | swereg − truth | swereg − TE |
|:-----|-------------------------------:|-------:|---------------:|---------------:|------------:|
| 3001 |                          0.393 |  0.479 |          0.502 |         +0.086 |      -0.023 |
| 3002 |                          0.393 |  0.463 |          0.474 |         +0.070 |      -0.011 |
| 3003 |                          0.393 |  0.463 |          0.478 |         +0.070 |      -0.014 |

Table 8. Harmful effect (true log-IRR \> 0) with strong depletion of
susceptibles, ITT with truncated weights, three seeds. Log-IRR scale.

Under a harmful effect with strong depletion of susceptibles, the
marginal hazard ratio declines over follow-up. The single pooled IRR is
a person-time-weighted average (1.6). It therefore lies above the
cumulative-rate truth, by a mean of +0.075 across the three seeds. This
is a property of the estimand, not an implementation defect. Both swereg
and `TrialEmulation` target the same weighted-average summary, so they
agree to within 0.023 on every seed. Analyses in which the time path of
the effect matters should report follow-up-specific estimates.

| Truncation percentiles (%) | True log-IRR | Estimate | Bias (attenuation) | Max raw stabilised weight |
|:---------------------------|-------------:|---------:|-------------------:|--------------------------:|
| 0.5 / 99.5                 |       -0.444 |   -0.366 |             +0.079 |                      1325 |
| 1.0 / 99.0                 |       -0.444 |   -0.330 |             +0.114 |                      1325 |
| 5.0 / 95.0                 |       -0.444 |   -0.226 |             +0.218 |                      1325 |

Table 9. Near-positivity violation: attenuation toward the null grows
with each tightening of the truncation percentiles. ITT, log-IRR scale.

Table 9 shows the cost side of the tradeoff stated in 1.8.3. Its design
has propensity scores near the boundary (maximum raw stabilised weight
1325). Each tightening of the truncation percentiles moves the estimate
further toward the null. When extreme weights are structural rather than
sporadic, the appropriate response is to restrict the eligible
population, not to truncate harder.

| Fit                                  | True log-IRR |       Estimate \[95% CI\] |   Bias | \|Bias\| |
|:-------------------------------------|-------------:|--------------------------:|-------:|---------:|
| pp, time-updated censoring covariate |       -1.195 | -1.066 \[-1.141, -0.992\] | +0.129 |    0.129 |
| pp, covariate frozen at baseline     |       -1.195 | -1.039 \[-1.112, -0.966\] | +0.156 |    0.156 |
| itt                                  |       -0.373 | -0.398 \[-0.442, -0.353\] | -0.024 |    0.024 |

Table 10. Treatment–confounder feedback: per-protocol bias with the
censoring covariate time-updated versus frozen at baseline; ITT for
reference. All fits use the primary truncated weights. Log-IRR scale.

The feedback cell marks the limit of the per-protocol estimator’s
validity. Here a time-varying confounder is affected by treatment and
drives both adherence and the outcome. IPCW with time-updated covariates
is less biased than IPCW with covariates frozen at baseline (\|bias\|
0.129 against 0.156). A bias remains, and the interval of the
time-updated fit excludes the truth. Part of that bias is the
working-model average under a time-ramping effect, and not selection.
The ITT estimand needs no censoring model against this feedback, and its
interval covers the truth in the same data (bias -0.024). Where
treatment–confounder feedback is central to the question, g-methods
beyond this pipeline are indicated, exactly as stated in 1.7.

### 3.5 Full-pipeline truth recovery (plan layer)

The layers above validate the estimators on pre-built person-period
panels. This layer validates everything that sits on top of them in
production:

- the machine-readable specification;

- enrollment-period assignment;

- sequential eligibility with a lifetime new-user exclusion;

- the 2:1 comparator draw in each enrollment period;

- the worker subprocess chain;

- the dual PP/ITT analysis files;

- the pooled weighted outcome model.

The data-generating process plants an exactly known truth in a realistic
skeleton. Persons are observed weekly from 2016-01-01 to 2021-06-30,
roughly 287 ISO weeks. That span deliberately includes the 53-week ISO
year 2020. Persons are split into never-treaters and initiators. An
initiator starts treatment at an enrollment period drawn uniformly from
the first 56 four-week enrollment periods. In the discontinuation cell,
an initiator stops after a geometric duration (4% weekly hazard).

The weekly outcome hazard is constant at 0.0025 untreated and doubled
while treated. The marginal per-week incidence rate ratio among
sustained users is therefore exactly 2.0. Scenario B adds a binary
frailty carried by 30% of persons. It doubles both the initiation
probability and the outcome hazard, so it is a genuine baseline
confounder. Mixture-averaging over the two risk groups with first-event
depletion attenuates the marginal truth to 1.982.

Loss, when present, is geometric. Its weekly hazard is 2%, or under
informative loss 1% in the low-risk group and 3% in the high-risk group.
Loss multiplies person-time equally in both arms. The truth is therefore
unchanged, and loss is purely a nuisance the machinery must tolerate.
The ITT truth in the discontinuation cell (1.44) is simulated directly.
It is the do(initiate)-versus-do(never) contrast with natural
discontinuation.

| Cell     | Scenario | Loss        | Persons | Person-weeks | Treated person-weeks | Events |
|:---------|:---------|:------------|--------:|-------------:|---------------------:|-------:|
| A_none   | A        | none        |   9,000 |    2,583,000 |              793,215 |  8,435 |
| A_indep  | A        | independent |  15,000 |      737,773 |               82,724 |  1,971 |
| A_inform | A        | informative |  15,000 |    1,138,270 |              196,791 |  3,368 |
| B_none   | B        | none        |   9,000 |    2,583,000 |              944,313 | 11,695 |
| B_indep  | B        | independent |  15,000 |      750,409 |              100,073 |  2,867 |
| B_inform | B        | informative |  15,000 |    1,154,908 |              189,498 |  3,879 |
| DISC     | A        | none        |   9,000 |    2,583,000 |              110,516 |  6,607 |

Table 11. Skeleton descriptives per plan-layer cell (before eligibility
and enrollment).

| Cell     | Loss        | PP truth |   PP IRR \[95% CI\] | covers | ITT truth |  ITT IRR \[95% CI\] | covers |
|:---------|:------------|---------:|--------------------:|:------:|----------:|--------------------:|:------:|
| A_none   | none        |     2.00 | 1.89 \[1.70, 2.10\] |  yes   |      2.00 | 1.89 \[1.70, 2.10\] |  yes   |
| A_indep  | independent |     2.00 | 2.20 \[1.78, 2.73\] |  yes   |      2.00 | 2.20 \[1.78, 2.73\] |  yes   |
| A_inform | informative |     2.00 | 2.24 \[1.90, 2.63\] |  yes   |      2.00 | 2.24 \[1.90, 2.63\] |  yes   |
| B_none   | none        |     1.98 | 2.03 \[1.81, 2.26\] |  yes   |      1.98 | 2.03 \[1.81, 2.26\] |  yes   |
| B_indep  | independent |     1.98 | 2.35 \[1.90, 2.90\] |  yes   |      1.98 | 2.37 \[1.92, 2.92\] |  yes   |
| B_inform | informative |     1.98 | 2.10 \[1.79, 2.47\] |  yes   |      1.98 | 2.04 \[1.73, 2.39\] |  yes   |
| DISC     | none        |     2.00 | 1.88 \[1.63, 2.18\] |  yes   |      1.44 | 1.24 \[1.10, 1.40\] |   no   |

Table 12. Plan-layer factorial (A = no confounding, B = baseline
confounding; each × no/independent/informative loss) plus the
discontinuation cell. PP and ITT estimates use the primary truncated
weights. IRR scale.

Two rows warrant comment. In the confounded no-loss cell (B_none) the
frailty does real confounding work. The crude rate ratio in the enrolled
ITT panel is 2.65, and the IPW-weighted rate ratio is 2.02, against a
marginal truth of 1.98. On the log scale, the weighting removes more
than 90% of the planted confounding. In the discontinuation cell the two
estimands separate in the direction of the truths. On the log scale, PP
− ITT = +0.418, against a true separation of +0.330. The per-protocol
analysis censors at deviation and reweights toward the
sustained-treatment truth of 2.0. Its estimate, 1.88 \[1.63, 2.18\], has
an interval that covers that truth. The ITT analysis keeps the
person-time after discontinuation and targets the do(initiate) truth of
1.44. In this single run its interval, 1.24 \[1.10, 1.40\], excludes
that truth. This cell also exercises the deviation rule of 1.4. Under
the per-protocol estimand, an event at or after the first discordant
week does not count.

A single pipeline run at a fixed seed cannot distinguish bias from
draw-level noise. The two no-loss scenarios are therefore repeated over
eight independent seeds at 6,000 persons each. Each replicate reruns the
complete pipeline:

| Scenario | Seed | Truth |   PP IRR \[95% CI\] | covers |  ITT IRR \[95% CI\] | covers |
|:---------|:-----|------:|--------------------:|:------:|--------------------:|:------:|
| A        | 5001 |  2.00 | 2.15 \[1.88, 2.46\] |  yes   | 2.15 \[1.88, 2.46\] |  yes   |
| A        | 5002 |  2.00 | 2.01 \[1.76, 2.30\] |  yes   | 2.01 \[1.76, 2.30\] |  yes   |
| A        | 5003 |  2.00 | 2.03 \[1.78, 2.31\] |  yes   | 2.03 \[1.78, 2.31\] |  yes   |
| A        | 5004 |  2.00 | 2.21 \[1.94, 2.50\] |  yes   | 2.21 \[1.94, 2.50\] |  yes   |
| A        | 5005 |  2.00 | 2.13 \[1.87, 2.42\] |  yes   | 2.13 \[1.87, 2.42\] |  yes   |
| A        | 5006 |  2.00 | 1.90 \[1.67, 2.16\] |  yes   | 1.90 \[1.67, 2.16\] |  yes   |
| A        | 5007 |  2.00 | 2.09 \[1.83, 2.39\] |  yes   | 2.09 \[1.83, 2.39\] |  yes   |
| A        | 5008 |  2.00 | 1.90 \[1.66, 2.16\] |  yes   | 1.90 \[1.66, 2.16\] |  yes   |
| B        | 5001 |  1.98 | 1.93 \[1.68, 2.23\] |  yes   | 1.93 \[1.68, 2.23\] |  yes   |
| B        | 5002 |  1.98 | 1.87 \[1.63, 2.15\] |  yes   | 1.87 \[1.63, 2.15\] |  yes   |
| B        | 5003 |  1.98 | 1.93 \[1.69, 2.22\] |  yes   | 1.93 \[1.69, 2.22\] |  yes   |
| B        | 5004 |  1.98 | 2.34 \[2.05, 2.68\] |   no   | 2.34 \[2.05, 2.68\] |   no   |
| B        | 5005 |  1.98 | 2.25 \[1.95, 2.60\] |  yes   | 2.25 \[1.95, 2.60\] |  yes   |
| B        | 5006 |  1.98 | 2.13 \[1.85, 2.45\] |  yes   | 2.13 \[1.85, 2.45\] |  yes   |
| B        | 5007 |  1.98 | 1.94 \[1.69, 2.22\] |  yes   | 1.94 \[1.69, 2.22\] |  yes   |
| B        | 5008 |  1.98 | 1.89 \[1.65, 2.16\] |  yes   | 1.89 \[1.65, 2.16\] |  yes   |

Table 13. Plan-layer Monte Carlo, per replicate; truncated (primary)
weights. IRR scale.

| Scenario | Estimand | Mean log bias | MC sd | 95% CI coverage |
|:---------|:---------|--------------:|------:|----------------:|
| A        | pp       |        +0.024 | 0.056 |             8/8 |
| A        | itt      |        +0.024 | 0.056 |             8/8 |
| B        | pp       |        +0.023 | 0.086 |             7/8 |
| B        | itt      |        +0.023 | 0.086 |             7/8 |

Table 14. Plan-layer Monte Carlo, summarised over the eight seeds;
truncated (primary) weights. Log-IRR scale.

In both scenarios and for both estimands, the mean log-scale bias is
within 3.5 Monte Carlo standard errors of zero. At least 6 of the 8
intervals cover the truth in each. At a true coverage of 95%, 5 or fewer
covering intervals of 8 has probability 0.006. Table 13 shows each miss.

### 3.6 Coverage calibration

The final layer asks whether the reported uncertainty can be trusted. It
draws 200 replicates per scenario at 3,000 persons and refits each one
end to end. It then counts the fraction of nominal 95% intervals that
cover the truth. The truth is the exact log-IRR of Section 3.9. Both
estimands use the primary truncated weight, the pipeline’s default
analysis as reported. The per-protocol censoring weights (1.8.2) target
the sustained-treatment effect in all three scenarios, including the
informative loss in s3. The ITT analysis carries no loss weight, so s3
also shows how its design bias affects its intervals.

| Scenario | Estimand | Nuisances                      | Replicates fit | Mean log bias | MC sd | 95% CI coverage |
|:---------|:---------|:-------------------------------|---------------:|--------------:|------:|----------------:|
| s1       | pp       | none                           |        200/200 |        -0.005 | 0.066 | 192/200 (96.0%) |
| s2       | pp       | confounding + independent loss |        200/200 |        +0.008 | 0.079 | 196/200 (98.0%) |
| s3       | pp       | confounding + informative loss |        200/200 |        +0.017 | 0.104 | 194/200 (97.0%) |
| s1       | itt      | none                           |        200/200 |        -0.005 | 0.060 | 192/200 (96.0%) |
| s2       | itt      | confounding + independent loss |        200/200 |        -0.001 | 0.078 | 192/200 (96.0%) |
| s3       | itt      | confounding + informative loss |        200/200 |        -0.027 | 0.093 | 189/200 (94.5%) |

Table 15. Coverage calibration against the exact log-IRR truth, M = 200
replicates per scenario and estimand at N = 3,000, using the primary
truncated weight, that is, the coverage of the pipeline’s default
analysis as reported. Log-IRR scale.

![Figure 3. Coverage calibration: all 200 replicate 95% confidence
intervals per scenario (per-protocol estimand, primary truncated
weights), sorted by point estimate, against the exact log-IRR truth
(horizontal line). Intervals that miss the truth are drawn in red. Table
15 gives the coverage of each
scenario.](tte-methods_files/figure-html/unnamed-chunk-32-1.png)

Figure 3. Coverage calibration: all 200 replicate 95% confidence
intervals per scenario (per-protocol estimand, primary truncated
weights), sorted by point estimate, against the exact log-IRR truth
(horizontal line). Intervals that miss the truth are drawn in red. Table
15 gives the coverage of each scenario.

The per-protocol interval stays close to nominal in all three scenarios:
96.0% in s1, 98.0% in s2 and 97.0% in s3. The largest departure from 95%
is 3.0 percentage points, against a binomial standard error of 1.5
points at 200 replicates. The point estimate carries a mean bias of
-0.005, +0.008 and +0.017 in s1, s2 and s3 (Table 15). Each bias is less
than a quarter of the spread of single estimates (MC sd 0.066 to 0.104),
so the intervals still cover near nominal.

The ITT interval covers 96.0% in s1 and 96.0% in s2. In s3 the ITT point
estimate carries the design bias of an analysis without loss weights.
Its mean bias is -0.027 (MC SE 0.007), and the exact limit is -0.027. At
N = 3,000 that bias is less than a third of the spread of single
estimates (MC sd 0.093). Coverage at this sample size therefore does not
show it. The validation tier therefore checks the s3 ITT bias against
its design limit, and not the s3 ITT coverage.

### 3.7 Marginal versus conditional estimands

Both swereg and `TrialEmulation` remove baseline confounding, but by
different routes. The two routes give two distinct estimands, and each
is valid. The swereg route weights and fits a covariate-free model,
which gives a marginal effect. The `TrialEmulation` route conventionally
adjusts the outcome model, which gives a conditional effect.

Rate ratios are collapsible, so the two coincide for the IRR. Odds
ratios are not collapsible, so the `TrialEmulation` OR is converted to a
rate ratio before comparison. The conversion uses the Zhang–Yu relation
${RR} = {OR}/\left( 1 - p_{0} + p_{0}\,{OR} \right)$, where $p_{0}$ is
the reference-arm per-period risk. The conversion removes the scale gap
only; a residual conditional-versus-marginal difference remains.

In Table 4 the swereg − TE gaps in the confounded ITT cells are small
(+0.006 in s2, +0.007 in s3). The per-protocol gaps are larger (+0.023
in s2, +0.061 in s3). In those cells the two packages also correct the
loss by different routes: censoring weights against conditioning. In s1,
which has no confounding, both gaps are below 0.001. The primary
correctness guarantee is each implementation’s agreement with the known
simulated truth on its own scale. Section 3.8 measures where each
route’s advantage holds, and where both end.

### 3.8 Boundary of validity: the truncation tradeoff across scenarios

The s3 per-protocol cell raised two questions that a single scenario
cannot answer. The first is whether the conditional-adjustment route is
always the better one. The second is whether truncation is always a
cost. This section varies the design one knob at a time around the s3
configuration:

- the strength of the dependence of loss on the confounder (0.45, 0.9
  and 1.5 on $L_{0}$);

- the direction of that dependence (−0.9, so that dropout selects
  low-risk rather than high-risk person-time);

- the direction of the treatment effect (harmful, $+ 0.7$);

- dropout driven by an unmeasured prognostic factor $U$;

- a healthy-adherer mechanism, in which treated individuals with high
  $U$ discontinue preferentially.

In a separate data-generating process, a time-varying covariate that
treatment itself affects drives the censoring. Every cell uses the
per-protocol estimand and ten paired replicates.

| Cell                                          | Datasets | Person-periods lost | True log-IRR | swereg truncated: mean bias (MC SE) | swereg untruncated: mean bias (MC SE) | TrialEmulation: mean bias (MC SE) |
|:----------------------------------------------|---------:|--------------------:|-------------:|------------------------------------:|--------------------------------------:|----------------------------------:|
| informative loss, mild (0.45·L0)              |       10 |                 51% |       -0.659 |                      +0.031 (0.008) |                        +0.001 (0.010) |                    -0.012 (0.008) |
| informative loss, base (0.9·L0, = s3)         |       10 |                 50% |       -0.659 |                      +0.041 (0.011) |                        +0.018 (0.027) |                    -0.019 (0.012) |
| informative loss, harsh (1.5·L0)              |       10 |                 50% |       -0.659 |                      +0.049 (0.013) |                        +0.275 (0.284) |                    +0.009 (0.010) |
| informative loss, reversed (−0.9·L0)          |       10 |                 50% |       -0.659 |                      +0.002 (0.008) |                        -0.031 (0.010) |                    -0.030 (0.007) |
| harmful effect (+0.7), informative loss       |       10 |                 50% |        0.639 |                      +0.040 (0.006) |                        -0.022 (0.034) |                    +0.030 (0.005) |
| unmeasured loss driver (0.9·U)                |       10 |                 50% |       -0.637 |                      -0.031 (0.011) |                        -0.045 (0.011) |                    -0.059 (0.012) |
| unmeasured adherence driver (healthy-adherer) |       10 |                  0% |       -0.637 |                      -0.013 (0.008) |                        -0.030 (0.009) |                    -0.050 (0.009) |

Table 16. Truncation-tradeoff grid, per-protocol estimand: one design
parameter at a time around the s3 configuration, plus two cells in which
selection is driven by an unmeasured prognostic factor U (N = 20,000 per
dataset). Log-IRR scale.

Some features of the s3 result generalise, and the ranking does not. The
grid cells use the Monte Carlo truth of Section 3.1, so their bias
includes the difference between the two truths. The truncated swereg fit
is above the truth in the mild, base and harsh cells (+0.031, +0.041,
+0.049; MC SE at most 0.013). Its 95% Monte Carlo interval excludes zero
in each, and its bias grows with how strongly the confounder drives the
loss. At the harshest setting the untruncated fit degrades: the standard
deviation of its estimates across datasets is 0.897, against 0.041 for
the truncated fit. Under extreme selection the censoring weights become
difficult to estimate. The conditional route is `TrialEmulation`, with
no censoring weights and the baseline covariate in the outcome model.
Its 95% Monte Carlo interval holds zero in these three cells. That holds
only because the covariate it conditions on drives this loss exactly.

No uniform ranking generalises. With the selection reversed, only the
truncated fit’s 95% Monte Carlo interval holds zero (+0.002, MC SE
0.008). The untruncated fit (-0.031, MC SE 0.010) and `TrialEmulation`
(-0.030, MC SE 0.007) are below zero. With a harmful effect, only the
untruncated fit’s interval holds zero (-0.022, MC SE 0.034). The
truncated fit (+0.040, MC SE 0.006) and `TrialEmulation` (+0.030, MC SE
0.005) are above the truth. No fit has the smallest absolute bias in
every cell.

The two unmeasured-driver cells locate the boundary set by assumption
(5) of the analysis plan (1.7). In both, every mean bias is negative,
toward an exaggerated protective effect. The `TrialEmulation` fit is
displaced the most: -0.059 with dropout on the unmeasured factor, and
-0.050 with the unmeasured factor driving adherence. Its 95% Monte Carlo
interval excludes zero in both. The swereg mean biases are -0.031
(truncated) and -0.045 (untruncated) in the dropout cell, and -0.013 and
-0.030 in the adherence cell. No weighting or conditioning on measured
covariates corrects selection on an unobserved variable. Two further
observations. First, the `TrialEmulation` displacement in the
unmeasured-loss cell contradicts an intuition: dropout independent of
treatment should cancel between arms in a ratio. Events deplete
high-risk person-time faster in the comparator arm. An identical dropout
process therefore interacts differently with the two arms’ risk sets,
and the ratio does not escape. Second, truncated and untruncated mean
biases differ less in these cells (0.014 and 0.017) than in the
measured-covariate cells (0.022 to 0.226). The
truncated-versus-untruncated divergence responds to weight instability
from measured covariates. It remains largely silent about unmeasured
drivers. Their detection needs design-based approaches (negative-control
outcomes, sensitivity analyses for unmeasured selection) rather than
weight diagnostics.

| Datasets | True log-IRR | swereg IPCW, time-updated covariate: mean bias (MC SE) | swereg IPCW, covariate frozen at baseline: mean bias (MC SE) | TrialEmulation, baseline conditioning: mean bias (MC SE) |
|---------:|-------------:|-------------------------------------------------------:|-------------------------------------------------------------:|---------------------------------------------------------:|
|       10 |       -1.195 |                                         +0.163 (0.015) |                                               +0.190 (0.015) |                                           +0.183 (0.016) |

Table 17. Feedback boundary, per-protocol estimand: censoring driven by
a time-varying covariate that treatment affects (the 1.7 regime); both
swereg fits use the truncated (primary) product weight. Every approach
has an absolute mean bias 2.8 to 3.2 times the largest absolute mean
bias of the truncated swereg fit and of TrialEmulation in Table 16.
Log-IRR scale.

Table 17 is the boundary the SAP declares in 1.7, now measured. Here the
determinants of censoring are time-varying and affected by treatment.
Each of the three approaches is biased, more than 3.5 Monte Carlo
standard errors from zero: time-updated censoring weights, frozen
covariates and baseline conditioning. Each bias is 2.8 to 3.2 times the
largest absolute mean bias of the truncated swereg fit and of
`TrialEmulation` in Table 16 (0.059). The comparison leaves out the
untruncated fit, because its bias in the harsh cell comes from unstable
weights. The time-updated weights give the smallest of the three biases.
All three estimates are therefore unusable, and comparing them
identifies only which approach fails least, not an approach that works.
When a time-varying confounder is itself affected by earlier treatment,
valid estimation needs methods designed for that feedback. Examples are
the parametric g-formula and g-estimation of structural nested models
(Hernán and Robins 2016). This pipeline implements neither.

![Figure 4. Mean bias of the per-protocol log-IRR across every
validation cell, one panel per scenario, with 95% Monte Carlo intervals;
the vertical line marks zero. s1 to s3 have 20 datasets each, against
the exact truth. The Table 16 grid cells have 10 each, against the Monte
Carlo truth. Both swereg weight variants are shown together with
TrialEmulation as the conditional-adjustment reference (a different
estimation route, not a third weight variant: baseline covariate in the
outcome model, no censoring weights, odds ratios converted to the
rate-ratio scale). Truncation moves the mean bias up in the mild, base,
reversed and harmful-effect cells, and down in the harsh cell, where the
untruncated weights are unstable. It changes no estimate in s1. In the
reversed-selection cell only the truncated fit's interval holds zero,
and in the harmful-effect cell only the untruncated fit's interval
does.](tte-methods_files/figure-html/unnamed-chunk-42-1.png)

Figure 4. Mean bias of the per-protocol log-IRR across every validation
cell, one panel per scenario, with 95% Monte Carlo intervals; the
vertical line marks zero. s1 to s3 have 20 datasets each, against the
exact truth. The Table 16 grid cells have 10 each, against the Monte
Carlo truth. Both swereg weight variants are shown together with
TrialEmulation as the conditional-adjustment reference (a different
estimation route, not a third weight variant: baseline covariate in the
outcome model, no censoring weights, odds ratios converted to the
rate-ratio scale). Truncation moves the mean bias up in the mild, base,
reversed and harmful-effect cells, and down in the harsh cell, where the
untruncated weights are unstable. It changes no estimate in s1. In the
reversed-selection cell only the truncated fit’s interval holds zero,
and in the harmful-effect cell only the untruncated fit’s interval does.

![Figure 5. Spread of the same per-protocol estimates: the standard
deviation across replicate datasets, the sampling noise an analyst
running one study draws from. One panel per scenario, with bars anchored
at zero; TrialEmulation is shown as the conditional-adjustment
reference. Truncation reduces this component of error. In none of the 10
cells does the truncated fit have a larger spread than the untruncated
fit, to 0.001. The largest ratio of the untruncated to the truncated
spread, 21.7, is in the cell 'informative loss, harsh
(1.5·L0)'.](tte-methods_files/figure-html/unnamed-chunk-43-1.png)

Figure 5. Spread of the same per-protocol estimates: the standard
deviation across replicate datasets, the sampling noise an analyst
running one study draws from. One panel per scenario, with bars anchored
at zero; TrialEmulation is shown as the conditional-adjustment
reference. Truncation reduces this component of error. In none of the 10
cells does the truncated fit have a larger spread than the untruncated
fit, to 0.001. The largest ratio of the untruncated to the truncated
spread, 21.7, is in the cell ‘informative loss, harsh (1.5·L0)’.

![Figure 6. Root-mean-squared error, combining bias (Figure 4) and
spread (Figure 5) as the two components of the bias–variance tradeoff:
the expected error of a single study's per-protocol estimate, and the
criterion on which the primary analysis is chosen. One panel per
scenario, with bars anchored at zero; TrialEmulation is shown as the
conditional-adjustment reference. Of the two swereg variants, the
truncated fit has the lower error in 8 of the 10 cells. Its largest
advantages are where the untruncated weights are unstable (harsh and
harmful-effect cells). The untruncated fit has the lower error in the
remaining cells, where it is less biased. No estimation route, the
reference included, has the lowest error in every
cell.](tte-methods_files/figure-html/unnamed-chunk-44-1.png)

Figure 6. Root-mean-squared error, combining bias (Figure 4) and spread
(Figure 5) as the two components of the bias–variance tradeoff: the
expected error of a single study’s per-protocol estimate, and the
criterion on which the primary analysis is chosen. One panel per
scenario, with bars anchored at zero; TrialEmulation is shown as the
conditional-adjustment reference. Of the two swereg variants, the
truncated fit has the lower error in 8 of the 10 cells. Its largest
advantages are where the untruncated weights are unstable (harsh and
harmful-effect cells). The untruncated fit has the lower error in the
remaining cells, where it is less biased. No estimation route, the
reference included, has the lowest error in every cell.

The recommendation follows from Figure 6, not from either ingredient
alone (Figures 4 and 5). In none of the 10 per-protocol cells does
truncation raise the spread of single-dataset estimates, to 0.001. The
truncated fit has the lower RMSE in 8 of them. Neither variant is
uniformly better. Truncation has its largest RMSE advantages where the
untruncated weights are unstable, in the harsh and harmful-effect cells.
The pipeline’s convention is therefore retained on the evidence. The
truncated fit is the primary analysis: its largest RMSE across the 10
cells is 0.062, against 0.894 for the untruncated fit. The untruncated
fit is always exported alongside (1.8.3). A material divergence between
the two indicates that the censoring weights are unstable. Three
responses are then appropriate:

- a sensitivity analysis at looser truncation percentiles (Table 9
  quantifies the dose–response);

- restriction of the eligible population where extreme weights are
  structural;

- when the censoring drivers are time-varying and treatment-affected,
  the recognition that no weighting scheme in this pipeline suffices
  (Table 17).

### 3.9 Risk difference and log-IRR against exact truths

This section compares the risk difference at 5, 10 and 20 periods, and
the log-IRR, with exact truths. The cells, seeds and limits are those of
the test tiers. On every push, `test-validation-fast.R` runs s1 and the
s4 per-protocol cell. Every week, `test-validation-full.R` runs every
cell (Section 4.2).

No simulation enters the two truths:

- **Risk difference.** The cumulative risk of a first event by period
  $h$ under the strategy of the estimand, intervention minus comparator.
  A forward recursion over the equations of Section 3.2 computes it,
  with quadrature over the baseline covariates.

- **Log-IRR.** The treatment coefficient of swereg’s outcome model
  (1.8.4), fitted to the exact expected events and person-time of each
  arm and period. Each cell is weighted by the marginal probability of
  remaining uncensored in its arm (“stabilised” weighting). That
  probability is the numerator of the stabilised censoring weight, so
  the truth is the limit of swereg’s weighted fit when the weights are
  correct.

The stabilised log-IRR truth differs from the truth without censoring
wherever the hazard ratio changes over follow-up. The largest difference
is 0.064, in s3 ITT. In s1 the hazard ratio is constant, and the two
truths are equal.

Scenario s4 adds a second standard-normal baseline covariate, so that
each censoring cause has its own driver. The covariate $L_{0}$ drives
the arm, discordance in the first follow-up week and later deviation.
The covariate $L_{1}$ drives loss to follow-up. Both drive the outcome.
Because a person can deviate in the first follow-up week, s4 tests the
time-zero censoring model of 1.8.2.

| Scenario | Estimand | Weights     |        log-IRR |     RD, h = 5 |    RD, h = 10 |     RD, h = 20 |
|:---------|:---------|:------------|---------------:|--------------:|--------------:|---------------:|
| s1       | pp       | truncated   | -0.0043 (-0.7) | +0.0001 (0.1) | +0.0001 (0.1) | -0.0013 (-0.8) |
| s1       | pp       | untruncated | -0.0043 (-0.7) | +0.0001 (0.1) | +0.0001 (0.1) | -0.0013 (-0.8) |
| s1       | itt      | truncated   | -0.0030 (-0.6) | +0.0001 (0.1) | +0.0001 (0.1) | -0.0006 (-0.4) |
| s1       | itt      | untruncated | -0.0030 (-0.6) | +0.0001 (0.1) | +0.0001 (0.1) | -0.0006 (-0.4) |
| s2       | pp       | truncated   |  +0.0203 (2.1) | +0.0026 (2.1) | +0.0028 (1.3) |  +0.0097 (3.5) |
| s2       | pp       | untruncated |  +0.0069 (0.7) | +0.0019 (1.5) | +0.0007 (0.3) |  +0.0036 (1.3) |
| s2       | itt      | truncated   |  +0.0140 (1.7) | +0.0024 (1.9) | +0.0021 (1.0) |  +0.0048 (2.4) |
| s2       | itt      | untruncated |  +0.0064 (0.7) | +0.0016 (1.3) | +0.0009 (0.4) |  +0.0029 (1.4) |
| s3       | pp       | truncated   |  +0.0298 (5.4) | +0.0037 (4.9) | +0.0096 (8.9) |  +0.0185 (8.6) |
| s3       | pp       | untruncated |  +0.0016 (0.1) | +0.0003 (0.2) | +0.0013 (0.6) |  +0.0025 (0.5) |
| s3       | itt      | truncated   | -0.0258 (-4.9) | +0.0038 (5.2) | +0.0051 (5.6) | -0.0003 (-0.2) |
| s3       | itt      | untruncated | -0.0326 (-6.2) | +0.0032 (4.3) | +0.0042 (4.6) | -0.0018 (-1.3) |
| s4       | pp       | truncated   |  +0.0442 (6.5) | +0.0037 (3.8) | +0.0106 (4.7) |  +0.0186 (6.0) |
| s4       | pp       | untruncated |  +0.0014 (0.1) | +0.0008 (0.7) | +0.0024 (0.9) | -0.0015 (-0.3) |
| s4       | itt      | truncated   | -0.0146 (-2.7) | +0.0029 (4.5) | +0.0041 (3.1) | -0.0001 (-0.1) |
| s4       | itt      | untruncated | -0.0202 (-3.7) | +0.0023 (3.6) | +0.0031 (2.4) | -0.0016 (-0.9) |

Table 18. Mean bias against the exact truth, with z = bias / MC SE in
brackets. 60 datasets per cell at N = 20,000. RD is the risk difference,
intervention minus comparator, at period h.

In s1, every estimate in Table 18 is within 3.5 Monte Carlo standard
errors of its exact truth (largest \|z\| 0.77). That holds for both
estimands, both weights, the log-IRR and the risk difference at every
horizon. With untruncated weights, every per-protocol estimate in s1 to
s4 is within the same limit (largest \|z\| 1.47). The ITT estimates of
s2 are within it with both weights (largest \|z\| 2.38).

Truncation moves the per-protocol risk difference up, away from the
truth, in s2, s3 and s4. At h = 20 the truncated mean bias is +0.0097
against +0.0036 untruncated in s2, +0.0185 against +0.0025 in s3 and
+0.0186 against -0.0015 in s4. In s3 and s4 the truncated bias at h = 20
is more than 3.5 Monte Carlo standard errors from zero (z 8.6 and 6.0).
In s1, which has no confounding and no loss, truncation changes no mean
estimate by more than 0.0005. Section 3.8 weighs this bias against the
lower spread of the truncated fit.

The ITT analysis carries no loss weight and assumes loss independent of
the outcome (assumption 4 of 1.7). In s3 and s4 the loss depends on a
covariate that also drives the outcome. The ITT log-IRR is therefore
below the truth with both weights. With untruncated weights the bias is
-0.0326 in s3 and -0.0202 in s4. The exact limit of an ITT fit without
loss weights is -0.0268 and -0.0161 from the truth. Each estimate is
within 3.5 Monte Carlo standard errors of its limit. The bias comes from
the violated assumption, and not from an estimator defect.

| Scenario | Horizon h | 95% interval holds the truth |
|:---------|----------:|-----------------------------:|
| s1       |         5 |              188/200 (94.0%) |
| s1       |        10 |              189/200 (94.5%) |
| s1       |        20 |              186/200 (93.0%) |
| s2       |         5 |              189/200 (94.5%) |
| s2       |        10 |              193/200 (96.5%) |
| s2       |        20 |              193/200 (96.5%) |
| s4       |         5 |              191/200 (95.5%) |
| s4       |        10 |              191/200 (95.5%) |
| s4       |        20 |              190/200 (95.0%) |

Table 19. Coverage of the 95% percentile bootstrap interval of the
per-protocol risk difference, untruncated weights, 200 datasets per
scenario at N = 20,000, with 200 bootstrap replicates each.

At every horizon in s1, s2 and s4, the bootstrap interval holds the
exact truth in 93.0% to 96.5% of the datasets. The full test tier
requires 90% to 99%. At a true coverage of 95% and 200 datasets, the
binomial standard error is 1.5 percentage points.

------------------------------------------------------------------------

## 4. Implementation mapping

Section 1 names no code. This section names the function, argument,
option, column and test file behind each step of Section 1, and the
source of the validation evidence.

### 4.1 SAP step → code

| SAP               | Step                                                        | Implementation                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               |
|:------------------|:------------------------------------------------------------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Values, 1.1, 1.4  | Width of the enrollment period                              | `period_width` argument of [`tteplan_from_spec_and_registrystudy()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_from_spec_and_registrystudy.md), default `4L`. It also sets the width of each follow-up interval. [`vignette("tte-timing")`](https://papadopoulos-lab.github.io/swereg/articles/tte-timing.md) states the timing rules with worked examples                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 |
| 1.1               | Global and per-enrollment criteria                          | Global: `inclusion_criteria$isoyears`, `inclusion_criteria$criteria` and `exclusion_criteria`. Per enrollment: `additional_inclusion` (`age_range`, `isoyear_range`, `has_event`, washouts) and `additional_exclusion`, applied after the global criteria by [`tteplan_apply_exclusions()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_apply_exclusions.md). [`tteplan_read_spec()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_read_spec.md) refuses a second `age_range` in one enrollment, because every `age_range` writes the one column `eligible_age`. It also refuses a second `isoyear_range`, and an `isoyear_range` outside `inclusion_criteria$isoyears` (26.13.0). An `isoyear_range` writes a column of its own, so the age range still applies. Since 26.14.0, TARGET item 6a lists the criteria per enrollment, and the protocol table states an enrollment-level exclusion as “is TRUE”                                                                                                                                                                                                                                                                                                                                   |
| 1.1               | Look-back windows                                           | `window` takes a number of weeks, `"N year"` or `"N years"` (read as 52N weeks), or `lifetime_before_baseline`. A value of 99999 or more, `Inf` included, means lifetime. Since 26.12.0 the window counts calendar ISO weeks from `isoyearweek`, not rows ([`any_events_prior_to()`](https://papadopoulos-lab.github.io/swereg/reference/any_events_prior_to.md)). An exclusion with `window: lifetime_before_and_after_baseline` reads every row of the person, after baseline as well                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      |
| 1.1, 1.8.7        | Washout / new-user exclusion                                | A washout rule in any of the four rule blocks. `type: no_prior_value` keeps a person-week when no prior week in the window holds `value`. A missing value in the window makes the rule `NA`, and the week is then ineligible. `type: only_prior_value` keeps a person-week when every prior week in the window that holds an observation holds `value`, so it skips missing weeks                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            |
| 1.1               | Prevalent-user warning                                      | [`tteplan_validate_spec()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_validate_spec.md) warns when no washout covers the enrollment’s intervention level on the weekly rows of the first skeleton batch. A prevalent week is a week at that level after an earlier week of the same person at that level. A washout covers the enrollment when it makes every prevalent week ineligible. swereg skips the check and says so when no skeleton is loaded (`global_max_isoyearweek` supplied). Set `options(swereg.warn_prevalent_user = FALSE)` to silence it                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                |
| 1.2, 1.4          | Arm values and the weekly status                            | `treatment.implementation`: `variable`, `intervention_value` and `comparator_value`. The pipeline writes `rd_intervention`: `TRUE` at the intervention value, `FALSE` at the comparator value, `NA` otherwise. It also writes `eligible_valid_treatment`, which is `TRUE` where `rd_intervention` is not `NA`                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                |
| 1.3               | Arm classification, recruiting week                         | `R/tte_enrollment_periods.R`. Each confounder reaches the panel as `.tte_entry__<v>`, its value at the recruiting week. The panel column `enrollment_period_id` names the trial, and `period_id` names the calendar period of each follow-up interval                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        |
| 1.3               | Comparator draw                                             | `TTEPlan$s1_generate_enrollments_and_ipw()`. `comparator_to_intervention_ratio` (required; [`tteplan_read_spec()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_read_spec.md) stops without it) and `seed` (required; [`tteplan_read_spec()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_read_spec.md) stops without it) come from the YAML spec’s `treatment.implementation`. The draw takes `round(ratio * n)` comparators, `by = enrollment_period_id`                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    |
| 1.4               | Time-zero checks                                            | `R/tte_landmark_qualify.R`. The attrition steps are `landmark_candidates` (“Candidate person-trials, before the time-zero checks”), `landmark_observed` (“Not under observation at time zero”) and `landmark_event_free` (“Event before time zero”). The observation contract is `observed_var` on each enrollment                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           |
| 1.4               | Arm tolerance                                               | `intervention_tolerance_weeks` and `comparator_tolerance_weeks` on each enrollment, each a whole number of at least 0, default 0                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             |
| 1.4               | Follow-up stop events, ties                                 | The private `TTEEnrollment` method `s5_prepare_outcome()`, which `$s4_prepare_for_analysis()` calls. `.tte_deviation_boundary()` writes `weeks_to_protocol_deviation`, the left edge of the first discordant week beyond tolerance. A panel built outside `enroll()` reads the `tstart` of the first discordant interval instead. `.deviation_clip` is `NA` when the deviation is not strictly before every other stop. The horizon comes from `follow_up`. The administrative end comes from `global_max_isoyearweek` (default: the largest `isoyearweek` of the first skeleton file), passed to `TTEDesign` as `admin_censor_isoyearweek`                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  |
| 1.4               | Loss of observation                                         | `.tte_record_end_boundary()` writes `weeks_to_record_end`, and `.tte_observation_gap_boundary()` writes `weeks_to_observation_gap`, the first absent week after time zero (`.tte_first_gap_week()`). Under both estimands `.tte_gap_record_end()` moves the record end to that week, so the stop row gets `weeks_to_loss`, `censor_loss = 1` and `censor_this_period = 1`. A gap on the same week as a deviation is labelled loss. `.tte_deviation_boundary()` holds discordant runs only, and ITT sets `weeks_to_protocol_deviation` to `NA`. A panel that `enroll()` built with `observed_var` and that lacks `weeks_to_observation_gap` was enrolled before swereg 26.15.0. Its weekly rows are gone, so the gap cannot be recomputed. swereg then warns once. The warning says that gaps in observation cannot be detected in the panel, and that an outcome after such a gap may be counted. A re-run of s1 removes the limitation. `.tte_gap_record_end()` gives the warning for such a panel in memory. [`qs2_read()`](https://papadopoulos-lab.github.io/swereg/reference/qs2_read.md) refuses a stored enrollment from before schema 6, which includes every enrollment saved before 26.15.0, so such a file cannot be read. Rebuild the plan with s0 and re-run s1 |
| 1.8.1             | Stabilised IPW                                              | `TTEEnrollment$s2_ipw(stabilize = TRUE)`, on the `.tte_entry__<v>` columns                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
| 1.8.2             | IPCW censoring models                                       | The private `TTEEnrollment` method `s6_ipcw_pp()`, reached through `$s4_prepare_for_analysis(estimate_ipcw_pp_with_gam = TRUE, estimate_ipcw_pp_separately_by_treatment = TRUE)`. Three models per stratum. Loss: indicator `censor_loss`, on the rows with `event == 0`. Deviation: indicator `censor_deviation`, on the rows with `event == 0` and `censor_loss == 0`. Both use the GAM engine `mgcv::bam(..., discrete = TRUE)`. When its prediction stops with an “object not found” error, swereg fits the same formula again with `discrete = FALSE`. Other prediction errors stop s2. The denominator is `flex(tstart) + flex(period_id) + confounders`, numerator `flex(tstart)`, each with `offset(log(person_weeks))`. `flex()` is `.tte_time_term(var, n, gam = TRUE)`. It gives `s(var)` with 10 or more distinct values, `splines::ns(var, df = 3)` with 4 to 9, `factor(var)` with 2 or 3, none with 1. Each count is over the risk set of the cause, after zero-width rows leave. Time zero: `stats::glm(deviation_time_zero ~ confounders, family = binomial())` on `$time_zero_deviation`, numerator the stratum’s proportion. `$ipcw_formulas[[stratum]][[cause]]` records each model, or `list(fitted = FALSE, reason = )` for a cause that fits none.    |
| 1.8.3             | Weight truncation                                           | In the pipeline, `TTEEnrollment$s3_truncate_weights(weight_cols = "ipw")` truncates the treatment weight at its defaults `lower = 0.01` and `upper = 0.99`, and writes `ipw_trunc` (ITT). The private `s6_ipcw_pp()` writes `analysis_weight_pp_trunc` (PP product weight) with 0.01 and 0.99 fixed in the code. Untruncated PP results are exported as a sensitivity sheet                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  |
| 1.8.4, 1.8.6      | Outcome model and inference                                 | `TTEEnrollment$irr(weight_col)`: `survey::svydesign(ids = ~person)` and `survey::svyglm(family = quasipoisson())` with `treatment + flex(tstart) + flex(enrollment_period_id) + offset(log(person_weeks))`. `flex()` is `.tte_time_term(var, n, gam = FALSE)`: `splines::ns(var, df = 3)` with 4 or more distinct values in the fitted rows, `factor(var)` with 2 or 3, none with 1. `tstart` is read through `.tte_interval_start()`. The result carries the formula in `attr(, "model_formula")`. The Wald interval uses `qnorm(1 - (1 - conf_level) / 2)`, and `$s3_analyze()` passes the study level `study.implementation.conf_level`                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
| 1.6, 1.8.5, 1.8.6 | Risk difference, number needed to treat, bootstrap interval | `TTEEnrollment$risk_difference(weight_col, n_boot, seed, conf_level)`. `$s3_analyze()` runs it on every ETT at 500 replicates and seed 1, on `analysis_weight_pp_trunc` (stored as `rd_pp_trunc` and `rd_curve_pp_trunc`) and on `ipw_trunc` (stored as `rd_itt` and `rd_curve_itt`). The level comes from `study.implementation.conf_level` in the YAML spec, default 0.95. `TTEPlan$get_curves()` returns the stop time in weeks from time zero as the column `follow_up_interval`, and the stored risk-difference rows use the same name                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  |
| 1.8.7             | Missing data                                                | `TTEEnrollment$s1_impute_confounders(seed = 4)`. The method is the plan’s `impute_fn`, whose default [`tteenrollment_impute_confounders()`](https://papadopoulos-lab.github.io/swereg/reference/tteenrollment_impute_confounders.md) draws one hot-deck value. `TTEEnrollment$s1b_fill_followup_confounders()` carries a follow-up value forward, and [`tteenrollment_fill_summary()`](https://papadopoulos-lab.github.io/swereg/reference/tteenrollment_fill_summary.md) counts the filled rows                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             |
| 1.8.8             | Subgroups and heterogeneity                                 | `subgroups` in the YAML spec, each with `implementation$variable`. `$s3_analyze()` runs `TTEEnrollment$irr_by_subgroup()` and `TTEEnrollment$effect_modification_test()` for each subgroup and estimand. `$irr_by_subgroup()` and `$effect_modification_test()` take `conf_level`. The effect-modification model is `treatment * factor(subgroup) + flex(tstart) + flex(enrollment_period_id)`. `TTEEnrollment$heterogeneity_test(weight_col)` fits `treatment * splines::ns(enrollment_period_id, df = min(3, n - 1)) + flex(tstart)`, where `n` is the number of trials; `$s3_analyze()` does not call it                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  |
| 1.9               | Sensitivity analyses                                        | The untruncated PP IRR is stored as `irr_pp`. `estimate_ipcw_pp_with_gam = FALSE` in `$s2_generate_analysis_files_and_ipcw_pp()` fits the loss and deviation models as cloglog GLMs, `.tte_time_term(var, n, gam = FALSE)` for `tstart` and `period_id`: `splines::ns(var, df = 3)` with 4 or more distinct values, `factor(var)` with 2 or 3, none with 1                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
| 1.8               | Pre-specification                                           | YAML spec parsed by [`tteplan_read_spec()`](https://papadopoulos-lab.github.io/swereg/reference/tteplan_read_spec.md); full grid run by `TTEPlan$s1_…`/`s2_…`/`s3_analyze()`                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 |

### 4.2 Where the validation numbers come from

The evidence layers in Section 3 are permanent, executable tests:

| Section | Layer                   | Test file                                     | Gate                                                                 |
|:--------|:------------------------|:----------------------------------------------|:---------------------------------------------------------------------|
| 3.3     | Cross-package triangle  | `tests/testthat/test-tte_validation_matrix.R` | runs in CI                                                           |
| 3.4     | Stress matrix           | `tests/testthat/test-tte_stress_matrix.R`     | fast subset in CI; full battery `SWEREG_RUN_VALIDATION=true`         |
| 3.5     | Plan-layer truth matrix | `tests/testthat/test-tteplan_truth_matrix.R`  | reduced-N subset in CI; full factorial `SWEREG_RUN_PLAN_MATRIX=true` |
| 3.6     | Coverage calibration    | `tests/testthat/test-tte_coverage.R`          | `SWEREG_RUN_VALIDATION=true`                                         |
| 3.9     | Exact truths, fast tier | `tests/testthat/test-validation-fast.R`       | runs in CI, inside `R CMD check`                                     |
| 3.9     | Exact truths, full tier | `tests/testthat/test-validation-full.R`       | `SWEREG_RUN_VALIDATION=true`                                         |

The workflow `.github/workflows/validation.yml` sets
`SWEREG_RUN_VALIDATION` and `SWEREG_RUN_PLAN_MATRIX`. It runs weekly, on
`v*` tags and on demand.

The tables and figures are rendered from
`vignettes/tte-validation-evidence.rds`, which
`dev/generate_validation_evidence.R` writes (in the source repository,
not the installed package). The script uses the same data-generating,
truth and fit helpers as the tests (`tests/testthat/helper-tte_*.R`).
Two tests read the `.rds` file:

- `test-validation-evidence-version.R` fails when `$meta$estimator_hash`
  differs from the md5 of the estimator source files. It runs in CI.
  Regenerate the evidence when an estimator file changes.
- `test-vignette-claims.R` checks each qualitative claim of Section 3
  and of the passages that cite it. Each claim is a named predicate in
  `vignettes/validation-claims.R`, and this vignette stops building when
  one is false. It runs in CI.

Rerun the script after any estimator change, and commit the refreshed
file with the change.

### References

- Hernán MA, Alonso A, Logan R, Grodstein F, Michels KB, Willett WC,
  Manson JE, Robins JM. Observational studies analyzed like randomized
  experiments: an application to postmenopausal hormone therapy and
  coronary heart disease. *Epidemiology* 2008;19(6):766–779. DOI
  10.1097/EDE.0b013e3181875e61.
- Hernán MA, Robins JM. Using big data to emulate a target trial when a
  randomized trial is not available. *Am J Epidemiol*
  2016;183(8):758–764. DOI 10.1093/aje/kwv254.
- Danaei G, García Rodríguez LA, Cantero OF, Logan R, Hernán MA.
  Observational data for comparative effectiveness research: an
  emulation of randomised trials of statins and primary prevention of
  coronary heart disease. *Stat Methods Med Res* 2013;22(1):70–96. DOI
  10.1177/0962280211403603.
- Altman DG. Confidence intervals for the number needed to treat. *BMJ*
  1998;317(7168):1309–1312. DOI 10.1136/bmj.317.7168.1309.
- Benchimol EI, et al. The REporting of studies Conducted using
  Observational Routinely-collected health Data (RECORD) Statement.
  *PLoS Med* 2015;12(10):e1001885. DOI 10.1371/journal.pmed.1001885.
- Caniglia EC, Zash R, Fennell C, et al. Emulating target trials to
  avoid immortal time bias: an application to antibiotic initiation and
  preterm delivery. *Epidemiology* 2023;34(3):430–438. DOI
  10.1097/EDE.0000000000001601.
- Dafni U. Landmark analysis at the 25-year landmark point. *Circ
  Cardiovasc Qual Outcomes* 2011;4(3):363–371. DOI
  10.1161/CIRCOUTCOMES.110.957951.
- Cashin AG, Hansford HJ, Hernán MA, et al. Transparent Reporting of
  Observational Studies Emulating a Target Trial: the TARGET Statement.
  *JAMA* 2025;334(12):1084–1093. DOI 10.1001/jama.2025.13350. Also *BMJ*
  2025;390:e087179.
- Thompson WA Jr. On the treatment of grouped observations in life
  studies. *Biometrics* 1977;33(3):463–470.
- Su L, Rezvani R, Seaman SR, Starr C, Gravestock I. TrialEmulation: An
  R Package to Emulate Target Trials for Causal Analysis of
  Observational Time-to-event Data. arXiv:2402.12083, 2024.
- Zhang J, Yu KF. What’s the relative risk? A method of correcting the
  odds ratio in cohort studies of common outcomes. *JAMA*
  1998;280(19):1690–1691.
