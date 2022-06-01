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

## Download Wikipedia Documents
1. For the `positive corpus`, define the Wikipedia categories of interest as an array assigned to `positive_domain_set` (line 99).
  * Use actual wikipedia categories, e.g. Law -> https://en.wikipedia.org/wiki/Category:Law
2. For the `negative corpus`, assign Wikipedia categories to `negative_domain_set` (line 100), that are either:
  * Semantically orthogonal to the `positive corpus` categories, for creating a topic boundary.
  * Semantically tangential to the `positive corpus` categories, for narrowing a specific topic.
    * Warning: use with care. Might be better to create a synthetic corpus using a controlled vocabulary.
3. Define the number of drill-down iterations in `domain_set_degrees` (line 102) for every Wikipedia category in `positive corpus` and `negative corpus`.
4. Specifically assign Wikipedia categories to `blacklist_subcats` (line 103) to ignore in the drill-down iterations.
5. Define string matching patterns in `negative_blacklist_substrings` (line 108) for Wikipedia categories to skip and not donwload their pages.
6. Define the name of the root node category in `parent_category` (line 112).
