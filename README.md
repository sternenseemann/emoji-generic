# emoji utilities for Haskell

This library is still growing! :sunflower:

## Progress

- [ ] Collect Data about Emojis
  - [x] `emoji-test.txt`
  - [ ] `emoji-data.txt`
  - [ ] CLDR (?) data to get missing info, like unicode names (data files only contain cldr short names)
- [ ] Collect Data about legal sequences (?)
  - [ ] `emoji-variation-sequences.txt`
  - [ ] `emoji-zwj-sequences.txt`
  - [ ] `emoji-sequences.txt`
- [ ] Fast runtime emoji lookup
  - [ ] Precompiled map using TemplateHaskell?
  - [ ] Short name → Emoji
  - [ ] Unicode name → Emoji
  - [ ] Keyboard group → list of Emoji
- [ ] Emoji colon sequence substitution based on short names

