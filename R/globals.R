## Silence R CMD check "no visible binding for global variable" NOTEs: these names
## are data-frame columns used inside dplyr/data-masking calls, not real globals.
utils::globalVariables(c(
  ".badlv",".badsq",".empty",".lv",".pc",".sq",".sqn",".w","Jepson","JepsonReformat",
  "POStags","POStags2","POStags3","POStags4","PatentClaim","accuracy","beauregard",
  "beginIn","benchmarkCat","benchmarkProcess","benchmarkProdByProcess","benchmarkProduct",
  "bodyLinesElement","bodyLinesStep","bodyLinesTotal","bodyType","bulletpoints.LETTERS",
  "bulletpoints.LETTERSRight","bulletpoints.brackets","bulletpoints.doublebrackets",
  "bulletpoints.letters","bulletpoints.lettersRight","bulletpoints.numbers",
  "bulletpoints.numbersRight","bulletpoints.roman.LETTERS","bulletpoints.roman.lower",
  "bulletpoints.semicolon","claimType","clnum","dependency","id","improv","improvement",
  "inBegin","independent","isItem","isMeans","isMeansLine","isSaid","isStep","label",
  "level","levelselect","lineType","mdep","meansCountLine","nElement","nStep","nTotal",
  "patent_id","pcid","preambleTerm","preambleTextStub","preambleType","proc","processBody",
  "processPreamble","processSimple","prodByProcess","short","singleLine","singleReformat",
  "split.processNoun","testClaim","text","useCount","useGerund","useIng","useMeans",
  "useNounVBG","useSaid","wordcount"
))
