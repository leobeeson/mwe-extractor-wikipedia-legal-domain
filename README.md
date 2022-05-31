# MWE Extractor from Wikipedia Documents - Law Categories

## Behaviour Overview
1. Download Wikipedia documents from user-defined categories.
  * User can define categories for a `positive corpus` and categories for a `negative corpus`.
  * Categories can be recursively traversed by a user-defined `degree` parameter, for both categories from the `positive corpus` as well as from the `negative corpus`.
2. Train a `MWE model` (Multi-Word Expressions, i.e. collocations) from the `positive corpus`.
  * Uses [Quanteda](https://quanteda.io/) for training the `MWE model`.
3. Train a `Keyness Model` to identify `MWE` highly associated to the individual, user-defined categories.
  * Uses `positive corpus` for categories of interest.
  * Uses `negative corpus` to remove noisy `MWE` not particularly specific to the categories in the `positive corpus`.
  