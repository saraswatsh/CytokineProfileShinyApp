# Example Cytokine Profiling Data 5.

Contains observed concentrations of cytokines and their respective
treatment and groups, derived from:

## Usage

``` r
ExampleData5
```

## Format

A data frame with 297 rows and 29 columns:

- Group:

  Group assigned to the subjects.

- Treatment:

  Treatment received by subjects.

- Batch:

  Batch number corresponding to the sample.

- Time:

  Time point of the measurement.

- IL.17F:

  Observed concentration of IL.17F cytokine.

- GM.CSF:

  Observed concentration of GM.CSF cytokine.

- IFN.G:

  Observed concentration of IFN.G cytokine.

- IL.10:

  Observed concentration of IL.10 cytokine.

- CCL.20.MIP.3A:

  Observed concentration of CCL.20.MIP.3A cytokine.

- IL.12.P70:

  Observed concentration of IL.12.P70 cytokine.

- IL.13:

  Observed concentration of IL.13 cytokine.

- IL.15:

  Observed concentration of IL.15 cytokine.

- IL.17A:

  Observed concentration of IL.17A cytokine.

- IL.22:

  Observed concentration of IL.22 cytokine.

- IL.9:

  Observed concentration of IL.9 cytokine.

- IL.1B:

  Observed concentration of IL.1B cytokine.

- IL.33:

  Observed concentration of IL.33 cytokine.

- IL.2:

  Observed concentration of IL.2 cytokine.

- IL.21:

  Observed concentration of IL.21 cytokine.

- IL.4:

  Observed concentration of IL.4 cytokine.

- IL.23:

  Observed concentration of IL.23 cytokine.

- IL.5:

  Observed concentration of IL.5 cytokine.

- IL.6:

  Observed concentration of IL.6 cytokine.

- IL.17E.IL.25:

  Observed concentration of IL.17E.IL.25 cytokine.

- IL.27:

  Observed concentration of IL.27 cytokine.

- IL.31:

  Observed concentration of IL.31 cytokine.

- TNF.A:

  Observed concentration of TNF.A cytokine.

- TNF.B:

  Observed concentration of TNF.B cytokine.

- IL.28A:

  Observed concentration of IL.28A cytokine.

## Source

Example data compiled for cytokine profiling.

## Note

The ExampleData5 dataset is the same data as ExampleData1 but with a new
column "Batch" added to indicate the batch number corresponding to each
sample. The "Batch" column was randomly generated to simulate batch
effects in the data.

## References

Pugh GH, Fouladvand S, SantaCruz-Calvo S, Agrawal M, Zhang XD, Chen J,
Kern PA, Nikolajczyk BS. T cells dominate peripheral inflammation in a
cross-sectional analysis of obesity-associated diabetes. *Obesity
(Silver Spring)*. 2022;30(10): 1983–1994. doi:10.1002/oby.23528.

## Examples

``` r
data(ExampleData5)
```
