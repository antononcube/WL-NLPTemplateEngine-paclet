(* Created by the Wolfram Language Plugin for IntelliJ, see http://wlplugin.halirutan.de/ *)

(* :Title: NLPTemplateEngineData *)
(* :Context: NLPTemplateEngineData` *)
(* :Author: Anton Antonov *)
(* :Date: 2021-09-02 *)

(* :Package Version: 0.5 *)
(* :Mathematica Version: 12.3 *)
(* :Copyright: (c) 2021 Anton Antonov *)
(* :Keywords: *)
(* :Discussion: *)


(***********************************************************)
(* Package definition                                      *)
(***********************************************************)

BeginPackage["AntonAntonov`NLPTemplateEngine`NLPTemplateEngineData`"];

(*NLPTemplateEngineData::usage = "NLPTemplateEngineData[] and NLPTemplateEngineData[(\"Standard\" | \"Default\"] \*)
(*gets the \"standard\" NLP template engine data.";*)

Begin["`Private`"];

Needs["AntonAntonov`NLPTemplateEngine`"];

(***********************************************************)
(* Shortcuts                                               *)
(***********************************************************)

aShortcuts = <|
  "ProgrammingEnvironment" -> "ProgrammingEnvironment",
  "Programming" -> "ProgrammingEnvironment",
  "System" -> "ProgrammingEnvironment",

  "QuantileRegression" -> "QuantileRegression",
  "QR" -> "QuantileRegression",

  "QRMon" -> "QRMon",

  "LatentSemanticAnalysis" -> "LatentSemanticAnalysis",
  "LSAMon" -> "LatentSemanticAnalysis",
  "LSA" -> "LatentSemanticAnalysis",

  "Classification" -> "Classification",
  "Classify" -> "Classification",

  "ClCon" -> "ClCon",
  "CLMon" -> "ClCon",

  "RandomTabularDataset" -> "RandomTabularDataset",
  "RandomDataset" -> "RandomTabularDataset",
  "RandomDataGeneration" -> "RandomTabularDataset",
  "RandomDatasetGeneration" -> "RandomTabularDataset",

  "Recommendations" -> "Recommendations",
  "SMRMon" -> "Recommendations",

  "NeuralNetworkCreation" -> "NeuralNetworkCreation"
|>;


(***********************************************************)
(* WL templates                                            *)
(***********************************************************)

aWLTemplates = <|
  "QuantileRegression" ->
      (StringTemplate @ StringReplace[#, "\n" ~~ (WhitespaceCharacter..) -> "\n"]&) @
          "Module[{qrData,aQRFuncs,aQRPlotData},
            qrData = `dataset`;
            qrData = N@Which[ Head[qrData] === TemporalData, QuantityMagnitude[qrData[\"Path\"]], VectorQ[qrData], Transpose[{Range@Length@qrData, qrData}], True, qrData];
            Echo[ResourceFunction[\"RecordsSummary\"][qrData],\"data summary:\"];
            aQRFuncs = AssociationThread[ `probs`, ResourceFunction[\"QuantileRegression\"][qrData, `knots`, `probs`, InterpolationOrder->`intOrder`]];
            aQRPlotData = Prepend[(Transpose[{qrData[[All, 1]], #1 /@ qrData[[All, 1]]}] &) /@ aQRFuncs, \"data\" -> qrData];
            Echo[ListPlot[Values[aQRPlotData], Joined -> Prepend[Table[True, Length[aQRPlotData]-1], False], PlotLegends -> Keys[aQRPlotData], PlotTheme -> \"Detailed\", FrameLabel -> {\"Regressor\", \"Value\"}, ImageSize -> Medium],\"regression quantiles:\"];
            Echo[Map[Function[{qFunc},
             DateListPlot[
              Map[{#[[1]], (qFunc[#[[1]]] - #[[2]])/#[[2]]} &, qrData], Joined -> False, PlotRange -> All, Filling -> Axis, PlotTheme -> \"Detailed\", ImageSize -> Medium]], aQRFuncs],\"errors:\"];
           ]",

  "QRMon" ->
      (StringTemplate @ StringReplace[#, "\n" ~~ (WhitespaceCharacter..) -> "\n"]&) @
          "qrObj=
        QRMonUnit[`dataset`]\[DoubleLongRightArrow]
        QRMonEchoDataSummary[]\[DoubleLongRightArrow]
        QRMonQuantileRegression[`knots`, `probs`, InterpolationOrder->`intOrder`]\[DoubleLongRightArrow]
        QRMonPlot[\"DateListPlot\"->`dateListPlotQ`,PlotTheme->\"Detailed\"]\[DoubleLongRightArrow]
        QRMonErrorPlots[\"RelativeErrors\"->`relativeErrorsQ`,\"DateListPlot\"->`dateListPlotQ`,PlotTheme->\"Detailed\"];",

  "LatentSemanticAnalysis" ->
      (StringTemplate @ StringReplace[#, "\n" ~~ (WhitespaceCharacter..) -> "\n"]&) @
          "lsaObj=
      LSAMonUnit[`textData`] \[DoubleLongRightArrow]
      LSAMonMakeDocumentTermMatrix[ \"StemmingRules\" -> `stemmingRules`, \"StopWords\" -> `stopWords`] \[DoubleLongRightArrow]
      LSAMonEchoDocumentTermMatrixStatistics[\"LogBase\" -> 10] \[DoubleLongRightArrow]
      LSAMonApplyTermWeightFunctions[\"GlobalWeightFunction\" -> \"`globalWeightFunction`\", \"LocalWeightFunction\" -> \"`localWeightFunction`\", \"NormalizerFunction\" -> \"`normalizerFunction`\"] \[DoubleLongRightArrow]
      LSAMonExtractTopics[\"NumberOfTopics\" -> `numberOfTopics`, Method -> \"`method`\", \"MaxSteps\" -> `maxSteps`, \"MinNumberOfDocumentsPerTerm\" -> `minNumberOfDocumentsPerTerm`] \[DoubleLongRightArrow]
      LSAMonEchoTopicsTable[\"NumberOfTerms\" -> `topicsTableNumberOfTerms`] \[DoubleLongRightArrow]
      LSAMonEchoStatisticalThesaurus[ \"Words\" -> `statThesaurusWords`];",

  "Classification" ->
      (StringTemplate @ StringReplace[#, "\n" ~~ (WhitespaceCharacter..) -> "\n"]&) @
          "Module[{clData,clDataTraining,clDataTesting,clObj,clCMObj,clMeasurements},
            clData = ClConToNormalClassifierData[`data`];
            {clDataTraining, clDataTesting} = TakeDrop[clData, Floor[`splitRatio` * Length[clData]]];
            clObj = Classify[clDataTraining, Method -> \"`method`\"];
            clCMObj = ClassifierMeasurements[clObj, clDataTesting];
            Echo[ clCMObj[{\"Accuracy\", \"Precision\", \"Recall\"}], \"measurements:\"];
            clMeasurements = Intersection[clCMObj[\"Properties\"], `measurementFuncs`];
            If[ Length[clMeasurements] > 0, Echo[ clCMObj[clMeasurements], ToString[clMeasurements] <> \":\"]];
            Echo[ clCMObj[\"ConfusionMatrixPlot\"], \"confusion matrix:\"];
           ]",

  "ClCon" ->
      (StringTemplate @ StringReplace[#, "\n" ~~ (WhitespaceCharacter..) -> "\n"]&) @
          "clObj=
        ClConUnit[`data`]\[DoubleLongRightArrow]
        ClConSplitData[`splitRatio`]\[DoubleLongRightArrow]
        ClConEchoDataSummary\[DoubleLongRightArrow]
        ClConMakeClassifier[\"`method`\"]\[DoubleLongRightArrow]
        ClConClassifierMeasurements[`measurementFuncs`]\[DoubleLongRightArrow]
        ClConEchoValue\[DoubleLongRightArrow]
        ClConROCPlot[`rocPlotFuncs`];",

  "RandomTabularDataset" ->
      StringTemplate[
        "ResourceFunction[\"RandomTabularDataset\"][" <>
            "{`nrow`, `ncol`}, " <>
            "\"ColumnNamesGenerator\" -> `columnNamesGenerator`, " <>
            "\"Form\" -> \"`form`\", " <>
            "\"MaxNumberOfValues\" -> `maxNumberOfValues`, " <>
            "\"MinNumberOfValues\" -> `minNumberOfValues`, " <>
            "\"RowKeys\" -> `rowKeys`" <>
            "]"],

  "Recommendations" ->
      (StringTemplate @ StringReplace[#, "\n" ~~ (WhitespaceCharacter..) -> "\n"]&) @
          "smrObj=
        SMRMonUnit[]\[DoubleLongRightArrow]
        SMRMonCreate[`dataset`]\[DoubleLongRightArrow]
        SMRMonRecommendByProfile[`prof`, `nrecs`]\[DoubleLongRightArrow]
        SMRMonJoinAcross[`dataset`]\[DoubleLongRightArrow]
        SMRMonEchoValue[];",

  "NeuralNetworkCreation" -> StringTemplate["\"Not implemented\""]
|>;


(***********************************************************)
(* R templates                                            *)
(***********************************************************)

aRTemplates = <|
  "QuantileRegression" ->
      StringTemplate["\"Not implemented\""],

  "QRMon" ->
      (StringTemplate @ StringReplace[#, "\n" ~~ (WhitespaceCharacter..) -> "\n"]&) @
          "qrObj <-
          QRMonUnit(`dataset`) %>%
          QRMonEchoDataSummary() %>%
          QRMonQuantileRegression(df = `knots`, probabilities = `probs`, degree = `intOrder`) %>%
          QRMonPlot(datePlotQ = `dateListPlotQ` ) %>%
          QRMonErrorsPlot(relativeErrors = `relativeErrorsQ`, datePlotQ = `dateListPlotQ`)",

  "LatentSemanticAnalysis" ->
      (StringTemplate @ StringReplace[#, "\n" ~~ (WhitespaceCharacter..) -> "\n"]&) @
          "lsaObj <-
          LSAMonUnit(`textData`) %>%
          LSAMonMakeDocumentTermMatrix(stemWordsQ = `stemmingRules`, stopWords = `stopWords`) %>%
          LSAMonEchoDocumentTermMatrixStatistics(logBase = 10) %>%
          LSAMonApplyTermWeightFunctions(globalWeightFunction = \"`globalWeightFunction`\", localWeightFunction = \"`localWeightFunction`\", normalizerFunction = \"`normalizerFunction`\") %>%
          LSAMonExtractTopics(numberOfTopics = `numberOfTopics`, method = \"`method`\", maxSteps = `maxSteps`, minNumberOfDocumentsPerTerm = `minNumberOfDocumentsPerTerm`) %>%
          LSAMonEchoTopicsTable(numberOfTerms = `topicsTableNumberOfTerms`, wideFormQ = TRUE) %>%
          LSAMonEchoStatisticalThesaurus(words = `statThesaurusWords`)",

  "Classification" -> StringTemplate["\"Not implemented\""],

  "ClCon" -> StringTemplate["\"Not implemented\""],

  "RandomTabularDataset" ->
      StringTemplate[
        "RandomDataFrame(" <>
            "nrow = `nrow`, ncol = `ncol`, " <>
            "columnNamesGenerator = `columnNamesGenerator`, " <>
            "form =  \"`form`\", " <>
            "maxNumberOfValues = `maxNumberOfValues`, " <>
            "minNumberOfValues = `minNumberOfValues`, " <>
            "rowNamesQ = `rowKeys`" <>
            ")"],

  "Recommendations" ->
      (StringTemplate @ StringReplace[#, "\n" ~~ (WhitespaceCharacter..) -> "\n"]&) @
          "smrObj <-
          SMRMonUnit() %>%
          SMRMonCreate( data = `dataset`) %>%
          SMRMonRecommendByProfile( profile = `prof`, nrecs = `nrecs`) %>%
          SMRMonJoinAcross( data = `dataset`) %>%
          SMRMonEchoValue()",

  "NeuralNetworkCreation" ->
      StringTemplate["\"Not implemented\""]
|>;


(***********************************************************)
(* Python templates                                        *)
(***********************************************************)

aPythonTemplates = <|
  "QuantileRegression" ->
      StringTemplate["\"Not implemented\""],

  "QRMon" ->
      (StringTemplate @ StringReplace[#, "\n" ~~ (WhitespaceCharacter..) -> "\n"]&) @
          "print(\"Example API -- no actual implementation !!!\")
          qrObj = QRMonUnit(`dataset`)
          QRMonEchoDataSummary(qrObj)
          qrObj = QRMonQuantileRegression(obj, df = `knots`, probabilities = `probs`, degree = `intOrder`)
          QRMonPlot(qrObj, datePlotQ = `dateListPlotQ` )
          QRMonErrorsPlot(qrObj, relativeErrors = `relativeErrorsQ`, datePlotQ = `dateListPlotQ`)",

  "LatentSemanticAnalysisImperative" ->
      (StringTemplate @ StringReplace[#, "\n" ~~ (WhitespaceCharacter..) -> "\n"]&) @
          "print(\"Example API -- no actual implementation !!!\")
          lsaObj = LSAMonUnit(`textData`)
          lsaObj = LSAMonMakeDocumentTermMatrix(lsaObj, stemWordsQ = `stemmingRules`, stopWords = `stopWords`)
          LSAMonEchoDocumentTermMatrixStatistics(lsaObj, logBase = 10)
          lsaObj = LSAMonApplyTermWeightFunctions(lsaObj, globalWeightFunction = \"`globalWeightFunction`\", localWeightFunction = \"`localWeightFunction`\", normalizerFunction = \"`normalizerFunction`\")
          lsaObj = LSAMonExtractTopics(lsaObj, numberOfTopics = `numberOfTopics`, method = \"`method`\", maxSteps = `maxSteps`, minNumberOfDocumentsPerTerm = `minNumberOfDocumentsPerTerm`)
          LSAMonEchoTopicsTable(lsaObj, numberOfTerms = `topicsTableNumberOfTerms`, wideFormQ = TRUE)
          LSAMonEchoStatisticalThesaurus(lsaObj, words = `statThesaurusWords`)",

  "LatentSemanticAnalysis" ->
      (StringTemplate @ StringReplace[#, "\n" ~~ (WhitespaceCharacter..) -> "\n"]&) @
          "lsaObj = (LatentSemanticAnalyzer()
          .make_document_term_matrix(docs=`textData`, stop_words=`stopWords`, stemming_rules=`stemmingRules`,min_length=3)
          .apply_term_weight_functions(global_weight_func='`globalWeightFunction`', local_weight_func='`localWeightFunction`',normalizer_func='`normalizerFunction`')
          .extract_topics(number_of_topics=`numberOfTopics`, min_number_of_documents_per_term=`minNumberOfDocumentsPerTerm`, method='`method`')
          .echo_topics_interpretation(number_of_terms=`topicsTableNumberOfTerms`, wide_form=True)
          .echo_statistical_thesaurus(terms=stemmerObj.stemWords(`statThesaurusWords`), wide_form=True, number_of_nearest_neighbors=12, method='cosine', echo_function=lambda x: print(x.to_string())))",

  "Classification" -> StringTemplate["\"Not implemented\""],

  "ClCon" -> StringTemplate["\"Not implemented\""],

  "RandomTabularDataset" ->
      StringTemplate[
        "random_data_frame(" <>
            "n_rows = `nrow`, columns_spec = `ncol`, " <>
            "column_names_generator = `columnNamesGenerator`, " <>
            "form =  \"`form`\", " <>
            "max_number_of_values = `maxNumberOfValues`, " <>
            "min_number_of_values = `minNumberOfValues`, " <>
            "row_names = `rowKeys`" <>
            ")"],

  "RecommendationsImperative" ->
      (StringTemplate @ StringReplace[#, "\n" ~~ (WhitespaceCharacter..) -> "\n"]&) @
          "print(\"Example API -- no actual implementation !!!\")
          smrObj = SMRMonUnit()
          smrObj = SMRMonCreate( data = `dataset`)
          smrObj = SMRMonRecommendByProfile(smrObj, profile = `prof`, nrecs = `nrecs`)
          smrObj = SMRMonJoinAcross(smrObj, data = `dataset`)
          SMRMonEchoValue(smrObj)",

  "Recommendations" ->
      (StringTemplate @ StringReplace[#, "\n" ~~ (WhitespaceCharacter..) -> "\n"]&) @
          "smrObj = (SparseMatrixRecommender()
              .create_from_wide_form(data = `dataset`, item_column_name=\"id\", columns=None, add_tag_types_to_column_names=True, tag_value_separator=\":\")
              .apply_term_weight_functions(\"IDF\", \"None\", \"Cosine\")
              .recommend_by_profile(profile=`prof`, nrecs=`nrecs`)
              .join_across(data=`dataset`, on=\"id\")
              .echo_value())",

  "NeuralNetworkCreation" ->
      StringTemplate["\"Not implemented\""]
|>;


(***********************************************************)
(* Raku templates                                          *)
(***********************************************************)

aRakuTemplates = <|
  "QuantileRegression" -> StringTemplate["\"Not implemented\""],

  "QRMon" -> StringTemplate["\"Not implemented\""],

  "LatentSemanticAnalysis" -> StringTemplate["\"Not implemented\""],

  "Classification" -> StringTemplate["\"Not implemented\""],

  "ClCon" -> StringTemplate["\"Not implemented\""],
  (* random-tabular-dataset(10, 3, max-number-of-values => 20)*)
  "RandomTabularDataset" ->
      StringTemplate[
        "random-tabular-dataset(" <>
            "`nrow`, `ncol`, " <>
            "column-names-generator => `columnNamesGenerator`, " <>
            "form =>  \"`form`\", " <>
            "max-number-of-values => `maxNumberOfValues`, " <>
            "min-number-of-values => `minNumberOfValues`, " <>
            "row-names => `rowKeys`" <>
            ")"],

  "RecommendationsImperative" ->
      (StringTemplate @ StringReplace[#, "\n" ~~ (WhitespaceCharacter..) -> "\n"]&) @
          "my ML::StreamsBlendingRecommender::CoreSBR $sbrObj .= new;
          $sbrObj.makeTagInverseIndexesFromWideForm(`dataset`);
          $sbrObj.normalizePerTagType();
          my $recs = $sbrObj.recommendByProfile( `prof`, `nrecs`):!object;
          silently { $recsTbl = $sbrObj.joinAcross($recs, @metadata, by=>Whatever):!object; }
          say to-pretty-table($recsTbl)",

  "Recommendations" ->
      (StringTemplate @ StringReplace[#, "\n" ~~ (WhitespaceCharacter..) -> "\n"]&) @
          "my ML::StreamsBlendingRecommender::CoreSBR $sbrObj .= new;
          $sbrObj.makeTagInverseIndexesFromWideForm(`dataset`).normalizePerTagType().recommendByProfile( `prof`, `nrecs`).joinAcross(`dataset`);
          say to-pretty-table($sbrObj.takeValue)",

  "NeuralNetworkCreation" ->
      StringTemplate["\"Not implemented\""]
|>;


(***********************************************************)
(* Swift templates                                         *)
(***********************************************************)

aSwiftTemplates = <|
  "QuantileRegression" -> StringTemplate["\"Not implemented\""],

  "QRMon" -> StringTemplate["\"Not implemented\""],

  "LatentSemanticAnalysis" -> StringTemplate["\"Not implemented\""],

  "Classification" -> StringTemplate["\"Not implemented\""],

  "ClCon" -> StringTemplate["\"Not implemented\""],
  (* random-tabular-dataset(10, 3, max-number-of-values => 20)*)
  "RandomTabularDataset" -> StringTemplate["\"Not implemented\""],

  "RecommendationsImperative" ->
      (StringTemplate @ StringReplace[#, "\n" ~~ (WhitespaceCharacter..) -> "\n"]&) @
          "let sbrObj : CoreSBR = CoreSBR()
          sbrObj.makeTagInverseIndexesFromWideForm(`dataset`)
          sbrObj.normalizePerTagType()
          let recs = sbrObj.recommendByProfile(`prof`, `nrecs`)
          let recsTbl = sbrObj.joinAcross(recs, metadata, by: \"ID\")
          print(recsTbl)",

  "Recommendations" -> StringTemplate["\"Not implemented\""],

  "NeuralNetworkCreation" ->
      StringTemplate["\"Not implemented\""]
|>;

aSwiftTemplates = Join[aSwiftTemplates, <| "Recommendations" -> aSwiftTemplates["RecommendationsImperative"]|> ];

(***********************************************************)
(* All templates                                           *)
(***********************************************************)

aTemplatesOrig = <| "Python" -> aPythonTemplates, "R" -> aRTemplates, "Raku" -> aRakuTemplates, "Swift" -> aSwiftTemplates, "WL" -> aWLTemplates |>;

aTemplates = Flatten @ Map[ Function[{key}, KeyValueMap[ {#1, key} -> #2 &, aTemplatesOrig[key] ]], Keys[aTemplatesOrig] ];
aTemplates = ResourceFunction["AssociationKeyDeflatten"][aTemplates];


(***********************************************************)
(* Questions                                               *)
(***********************************************************)

aQuestions = <|

  "ProgrammingEnvironment" ->
      (KeySort @
          Join[ #,
            KeyMap[ StringReplace[#, {"package" ~~ WordBoundary -> "library", "packages" -> "libraries"}]&, #],
            KeyMap[ StringReplace[#, {"use" -> "load"}]&, #],
            KeyMap[ StringReplace[#, {"language" -> "language to use"}]&, #],
            KeyMap[ StringReplace[#, {"Which" -> "What"}]&, #]
          ] & @
          <|
            "Which language" -> <|"TypePattern" -> _String, "Threshold" -> 0.56, "Parameter" -> "lang", "ContextWordsToRemove" -> {"code", "language", "programming"} |>,
            "Which programming language" -> <|"TypePattern" -> _String, "Threshold" -> 0.56, "Parameter" -> "lang", "ContextWordsToRemove" -> {"code", "language", "programming"} |>,
            "Which computer language" -> <|"TypePattern" -> _String, "Threshold" -> 0.56, "Parameter" -> "lang", "ContextWordsToRemove" -> {"code", "language", "programming"} |>,

            "Which packages" -> <|"TypePattern" -> _String, "Threshold" -> 0.56, "Parameter" -> "packages", "ContextWordsToRemove" -> {"library", "libraries", "package", "packages"} |>,
            "Which packages to use" -> <|"TypePattern" -> _String, "Threshold" -> 0.56, "Parameter" -> "packages", "ContextWordsToRemove" -> {"library", "libraries", "package", "packages"} |>
          |>),

  "QuantileRegression" ->
      <|
        "How many knots" -> <|"TypePattern" -> _Integer, "Threshold" -> 0.6, "Parameter" -> "knots",
          "ContextWordsToRemove" -> {"knots"} |>,

        "What is the interpolation order" -> <|"TypePattern" -> _Integer, "Threshold" -> 0.75, "Parameter" -> "intOrder"|>,

        "Data summary" -> <|"TypePattern" -> _?BooleanQ, "Threshold" -> 0.75, "Parameter" -> "dataSummaryQ"|>,

        "Which axes to rescale" -> <|"TypePattern" -> {_String ..}, "Threshold" -> 0.75, "Parameter" -> {"rescaleTimeAxisQ", "rescaleValueAxisQ"}|>,

        "What kind of plot" -> <|"TypePattern" -> _?BooleanQ, "Threshold" -> 0.75, "Parameter" -> "dateListPlotQ",
          "TrueValues" -> {"true", "yes", "datelist", "date list", "datelist plot", "use date list plot"}|>,
        "Date list plot" -> <|"TypePattern" -> _?BooleanQ, "Threshold" -> 0.75,
          "Parameter" -> "dateListPlotQ",
          "TrueValues" -> {"true", "yes", "datelist", "date list", "datelist plot", "use date list plot"}|>,

        "Relative errors plot" -> <|"TypePattern" -> _?BooleanQ, "Threshold" -> 0.75, "Parameter" -> "relativeErrorsQ"|>,
        "Absolute errors plot" -> <|"TypePattern" -> _?BooleanQ, "Threshold" -> 0.75, "Parameter" -> "absoluteErrorsQ"|>,

        "Which dataset to use" -> <|"TypePattern" -> _String, "Threshold" -> 0.40, "Parameter" -> "dataset",
          "ContextWordsToRemove" -> {"dataset", "data"}|>,
        "Which data to use" -> <|"TypePattern" -> _String, "Threshold" -> 0.40, "Parameter" -> "dataset",
          "ContextWordsToRemove" -> {"data", "dataset"}|>,
        "Which time series to use" -> <|"TypePattern" -> _String, "Threshold" -> 0.4, "Parameter" -> "dataset",
          "ContextWordsToRemove" -> {"time series"}|>,
        "Over which" -> <|"TypePattern" -> _String, "Threshold" -> 0.35, "Parameter" -> "dataset",
          "ContextWordsToRemove" -> {"dataset", "data"}|>,
        "Over what" -> <|"TypePattern" -> _String, "Threshold" -> 0.35, "Parameter" -> "dataset",
          "ContextWordsToRemove" -> {"dataset", "data"}|>,

        "Which probabilities" -> <|"TypePattern" -> {_?NumericQ ..}, "Threshold" -> 0.7, "Parameter" -> "probs",
          "ContextWordsToRemove" -> {"probabilities"}|>,
        "Which regression quantiles" -> <|"TypePattern" -> {_?NumericQ ..}, "Threshold" -> 0.6, "Parameter" -> "probs",
          "ContextWordsToRemove" -> {"regression quantiles"}|>
      |>,

  "LatentSemanticAnalysis" ->
      <|
        "Apply stemming" -> <|"TypePattern" -> _?BooleanQ, "Threshold" -> 0.75, "Parameter" -> "stemmingRules"|>,

        "How many topics" -> <|"TypePattern" -> _Integer, "Threshold" -> 0.45, "Parameter" -> "numberOfTopics",
          "ContextWordsToRemove" -> {"topics"}|>,
        "How many topics to extract" -> <|"TypePattern" -> _Integer, "Threshold" -> 0.45, "Parameter" -> "numberOfTopics",
          "ContextWordsToRemove" -> {"topics"}|>,

        "Which method" -> <|"TypePattern" -> _String, "Threshold" -> 0.5, "Parameter" -> "method",
          "ContextWordsToRemove" -> {"method", "algorithm"}|>,
        "Which dimension reduction method" -> <|"TypePattern" -> _String, "Threshold" -> 0.5, "Parameter" -> "method",
          "ContextWordsToRemove" -> {"method", "algorithm"}|>,
        "Which topic extraction method" -> <|"TypePattern" -> _String, "Threshold" -> 0.5, "Parameter" -> "method",
          "ContextWordsToRemove" -> {"method", "algorithm"}|>,

        "Show topics table" -> <|"TypePattern" -> _?BooleanQ, "Threshold" -> 0.75, "Parameter" -> "showTopicsTableQ"|>,
        "Number of terms in the topics table" -> <|"TypePattern" -> _?BooleanQ, "Threshold" -> 0.75, "Parameter" -> "topicsTableNumberOfTerms"|>,

        "Which words to use for statistical thesaurus" -> <|"TypePattern" -> {_String ..}, "Threshold" -> 0.15, "Parameter" -> "statThesaurusWords"|>,
        "Thesaurus words" -> <|"TypePattern" -> {_String ..}, "Threshold" -> 0.15, "Parameter" -> "statThesaurusWords"|>,
        "Thesaurus words to show" -> <|"TypePattern" -> {_String ..}, "Threshold" -> 0.15, "Parameter" -> "statThesaurusWords"|>,
        "Statistical thesaurus words" -> <|"TypePattern" -> {_String ..}, "Threshold" -> 0.75, "Parameter" -> "statThesaurusWords"|>,

        "Which dataset to use" -> <|"TypePattern" -> _String, "Threshold" -> 0.35, "Parameter" -> "textData"|>,
        "Which text corpus" -> <|"TypePattern" -> _String, "Threshold" -> 0.35, "Parameter" -> "textData"|>,
        "Which collection of texts" -> <|"TypePattern" -> _String, "Threshold" -> 0.35, "Parameter" -> "textData"|>
      |>,

  "Classification" ->
      <|
        "Which dataset to use" -> <|"TypePattern" -> _String, "Threshold" -> 0.35, "Parameter" -> "data", "ContextWordsToRemove" -> {"dataset", "data"}|>,
        "Which data" -> <|"TypePattern" -> _String, "Threshold" -> 0.35, "Parameter" -> "data", "ContextWordsToRemove" -> {"dataset", "data"}|>,
        "Which data to use" -> <|"TypePattern" -> _String, "Threshold" -> 0.35, "Parameter" -> "data", "ContextWordsToRemove" -> {"dataset", "data"}|>,
        "For which data" -> <|"TypePattern" -> _String, "Threshold" -> 0.35, "Parameter" -> "data", "ContextWordsToRemove" -> {"dataset", "data"}|>,
        "For which dataset" -> <|"TypePattern" -> _String, "Threshold" -> 0.35, "Parameter" -> "data", "ContextWordsToRemove" -> {"dataset", "data"}|>,
        "Using which dataset" -> <|"TypePattern" -> _String, "Threshold" -> 0.35, "Parameter" -> "data", "ContextWordsToRemove" -> {"dataset", "data"}|>,
        "Over which dataset" -> <|"TypePattern" -> _String, "Threshold" -> 0.35, "Parameter" -> "data", "ContextWordsToRemove" -> {"dataset", "data"}|>,

        "What is the split ratio" -> <|"TypePattern" -> _?NumericQ, "Threshold" -> 0.75, "Parameter" -> "splitRatio"|>,
        "Which split ratio to use" -> <|"TypePattern" -> _?NumericQ, "Threshold" -> 0.75, "Parameter" -> "splitRatio"|>,
        "Training vs testing data  ratio" -> <|"TypePattern" -> _?NumericQ, "Threshold" -> 0.75, "Parameter" -> "splitRatio"|>,

        "Which classifier method" -> <|"TypePattern" -> _String, "Threshold" -> 0.66, "Parameter" -> "method"|>,
        "What kind of classifier" -> <|"TypePattern" -> _String, "Threshold" -> 0.66, "Parameter" -> "method"|>,
        "How to classify" -> <|"TypePattern" -> _String, "Threshold" -> 0.66, "Parameter" -> "method"|>,
        "Which classifier algorithm" -> <|"TypePattern" -> _String, "Threshold" -> 0.66, "Parameter" -> "method"|>,

        "Classifier measurements" -> <|"TypePattern" -> {_String ..}, "Threshold" -> 0.35, "Parameter" -> "measurementFuncs",
          "ContextWordsToRemove" -> {"measurements", "ROC functions", "classifier"}|>,
        "Which evaluation metrics" -> <|"TypePattern" -> {_String ..}, "Threshold" -> 0.35, "Parameter" -> "measurementFuncs",
          "ContextWordsToRemove" -> {"measurements", "ROC functions", "classifier"}|>,
        "Which measurements" -> <|"TypePattern" -> {_String ..}, "Threshold" -> 0.35, "Parameter" -> "measurementFuncs",
          "ContextWordsToRemove" -> {"measurements", "ROC functions", "classifier"}|>,

        "Which ROC functions" -> <|"TypePattern" -> {_String ..}, "Threshold" -> 0.35, "Parameter" -> "rocPlotFuncs"|>,
        "Which ROC plot functions" -> <|"TypePattern" -> {_String ..}, "Threshold" -> 0.35, "Parameter" -> "rocPlotFuncs"|>
      |>,

  "RandomTabularDataset" ->
      <|
        "How many rows" -> <|"TypePattern" -> _Integer, "Threshold" -> 0.5, "Parameter" -> "nrow", "ContextWordsToRemove" -> {"row", "rows"}|>,
        "What number of rows" -> <|"TypePattern" -> _Integer, "Threshold" -> 0.5, "Parameter" -> "nrow",  "ContextWordsToRemove" -> {"row", "rows"}|>,
        "Rows count" -> <|"TypePattern" -> _Integer, "Threshold" -> 0.5, "Parameter" -> "nrow",  "ContextWordsToRemove" -> {"row", "rows"}|>,

        "How many columns" -> <|"TypePattern" -> _Integer, "Threshold" -> 0.5, "Parameter" -> "ncol", "ContextWordsToRemove" -> {"columns", "columns"}|>,
        "What number of columns" -> <|"TypePattern" -> _Integer, "Threshold" -> 0.5, "Parameter" -> "ncol",  "ContextWordsToRemove" -> {"columns", "columns"}|>,
        "Columns count" -> <|"TypePattern" -> _Integer, "Threshold" -> 0.5, "Parameter" -> "ncol", "ContextWordsToRemove" -> {"columns", "columns"}|>,

        "Max number values" -> <|"TypePattern" -> _Integer, "Threshold" -> 0.5, "Parameter" -> "maxNumberOfValues"|>,
        "Maximum number of values" -> <|"TypePattern" -> _Integer, "Threshold" -> 0.5, "Parameter" -> "maxNumberOfValues"|>,
        "Number of values" -> <|"TypePattern" -> _Integer, "Threshold" -> 0.5, "Parameter" -> "maxNumberOfValues"|>,
        "At most how many of values" -> <|"TypePattern" -> _Integer, "Threshold" -> 0.5, "Parameter" -> "maxNumberOfValues"|>,

        "Min number values" -> <|"TypePattern" -> _Integer, "Threshold" -> 0.5, "Parameter" -> "minNumberOfValues"|>,
        "Minimum number of values" -> <|"TypePattern" -> _Integer, "Threshold" -> 0.5, "Parameter" -> "minNumberOfValues"|>,
        "At least how many of values" -> <|"TypePattern" -> _Integer, "Threshold" -> 0.5, "Parameter" -> "minNumberOfValues"|>,

        "Which form" -> <|"TypePattern" -> _String, "Threshold" -> 0.5, "Parameter" -> "form"|>,
        "Which format" -> <|"TypePattern" -> _String, "Threshold" -> 0.5, "Parameter" -> "form"|>,
        "What kind of form" -> <|"TypePattern" -> _String, "Threshold" -> 0.5, "Parameter" -> "form"|>,
        "What kind of format" -> <|"TypePattern" -> _String, "Threshold" -> 0.5, "Parameter" -> "form"|>,

        "What is the column name generator" -> <|"TypePattern" -> _Integer, "Threshold" -> 0.5, "Parameter" -> "columnNamesGenerator"|>,
        "Which is the column name generator" -> <|"TypePattern" -> _Integer, "Threshold" -> 0.5, "Parameter" -> "columnNamesGenerator"|>,
        "How to generate the column names" -> <|"TypePattern" -> _Integer, "Threshold" -> 0.5, "Parameter" -> "columnNamesGenerator"|>,

        "What are the value generators" -> <|"TypePattern" -> {_String..}, "Threshold" -> 0.5, "Parameter" -> "generators"|>,
        "What are the generators" -> <|"TypePattern" -> {_String..}, "Threshold" -> 0.5, "Parameter" -> "generators"|>,
        "Which value generators" -> <|"TypePattern" -> {_String..}, "Threshold" -> 0.5, "Parameter" -> "generators"|>,
        "Which generators" -> <|"TypePattern" -> {_String..}, "Threshold" -> 0.5, "Parameter" -> "generators"|>
      |>,

  "Recommendations" ->
      <|
        "Which dataset to use" -> <|"TypePattern" -> _String, "Threshold" -> 0.35, "Parameter" -> "dataset"|>,
        "Which data" -> <|"TypePattern" -> _String, "Threshold" -> 0.35, "Parameter" -> "dataset"|>,
        "Which data to use" -> <|"TypePattern" -> _String, "Threshold" -> 0.35, "Parameter" -> "dataset"|>,
        "Over which data" -> <|"TypePattern" -> _String, "Threshold" -> 0.35, "Parameter" -> "dataset"|>,
        "Over which dataset" -> <|"TypePattern" -> _String, "Threshold" -> 0.35, "Parameter" -> "dataset"|>,

        "Which profile to use" -> <|"TypePattern" -> {_String ..}, "Threshold" -> 0.35, "Parameter" -> "prof"|>,
        "Which profile" -> <|"TypePattern" -> {_String ..}, "Threshold" -> 0.35, "Parameter" -> "prof"|>,
        "What is the profile" -> <|"TypePattern" -> {_String ..}, "Threshold" -> 0.35, "Parameter" -> "data"|>,

        "How many recommendations" -> <|"TypePattern" -> _Integer, "Threshold" -> 0.35, "Parameter" -> "nrecs"|>,
        "What number of top recommendations" -> <|"TypePattern" -> _Integer, "Threshold" -> 0.35, "Parameter" -> "nrecs"|>,
        "How many results" -> <|"TypePattern" -> _Integer, "Threshold" -> 0.35, "Parameter" -> "nrecs"|>,
        "What number of top results" -> <|"TypePattern" -> _Integer, "Threshold" -> 0.35, "Parameter" -> "nrecs"|>
      |>,

  "NeuralNetworkCreation" -> <||>
|>;

aQuestions = Join[aQuestions, <|"ClCon" -> aQuestions["Classification"], "QRMon" -> aQuestions["QuantileRegression"]|>];


(***********************************************************)
(* Defaults                                                *)
(***********************************************************)

aDefaults = <|
  "ProgrammingEnvironment" -> <|
    "Language" -> "WL",
    "Packages" -> None
  |>,

  "QuantileRegression" -> <|
    "knots" -> 12,
    "probs" -> {0.25, 0.5, 0.75},
    "intOrder" -> 3,
    "rescaleTimeAxisQ" -> False,
    "rescaleValueAxisQ" -> False,
    "dateListPlotQ" -> False,
    "dataset" -> None,
    "relativeErrorsQ" -> False
  |>,

  "LatentSemanticAnalysis" -> <|
    "globalWeightFunction" -> "IDF",
    "localWeightFunction" -> "None",
    "maxSteps" -> 16,
    "method" -> "SVD",
    "minNumberOfDocumentsPerTerm" -> 20,
    "normalizerFunction" -> "Cosine",
    "numberOfTopics" -> 40,
    "removeStopWordsQ" -> True,
    "showTopicsTableQ" -> True,
    "statThesaurusWords" -> None,
    "stemmingRules" -> Automatic,
    "stopWords" -> Automatic,
    "textData" -> None,
    "thesaurusWords" -> None,
    "topicsTableNumberOfTerms" -> 10|>,

  "Classification" -> <|
    "data" -> None,
    "splitRatio" -> 0.75,
    "method" -> "LogisticRegression",
    "measurementFuncs" -> "{\"Accuracy\", \"Precision\", \"Recall\"}",
    "rocPlotFuncs" -> "{\"FPR\", \"TPR\"}"
  |>,

  "RandomTabularDataset" -> <|
    "nrow" -> Automatic,
    "ncol" -> Automatic,
    "columnNamesGenerator" -> Automatic,
    "generators" -> Automatic,
    "form" -> "Wide",
    "maxNumberOfValues" -> Automatic,
    "minNumberOfValues" -> Automatic,
    "pointwiseGeneration" -> False,
    "rowKeys" -> False
  |>,

  "Recommendations" -> <|
    "dataset" -> None,
    "prof" -> None,
    "nrecs" -> 12
  |>,

  "NeuralNetworkCreation" -> <||>
|>;

aDefaults = Join[aDefaults, <|"ClCon" -> aDefaults["Classification"], "QRMon" -> aDefaults["QuantileRegression"]|>];


(***********************************************************)
(* NLPTemplateEngineData                                   *)
(***********************************************************)

Clear[NLPTemplateEngineData];

NLPTemplateEngineData[] := NLPTemplateEngineData["Standard"];

NLPTemplateEngineData[("Standard" | "Default")] :=
    <|
      "Templates" -> aTemplates,
      "Questions" -> aQuestions,
      "Defaults" -> aDefaults,
      "Shortcuts" -> aShortcuts
    |>;

End[];

EndPackage[];