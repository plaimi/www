{-
- Copyright (C) 2014 plaimi <www@plaimi.net>
-
- Copying and distribution of this file, with or without modification,
- are permitted in any medium without royalty provided the copyright
- notice and this notice are preserved.  This file is offered as-is,
- without any warranty.
-} module Config where

synchCommand ::  String
synchCommand = "rsync -av _site/"

synchTarget ::  String
synchTarget  = "plaimi:plaimi-www/"
