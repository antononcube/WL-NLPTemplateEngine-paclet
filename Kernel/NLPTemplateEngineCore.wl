(* Created by the Wolfram Language Plugin for IntelliJ, see http://wlplugin.halirutan.de/ *)

(* :Title: NLPTemplateEngine *)
(* :Context: NLPTemplateEngine` *)
(* :Author: Anton Antonov *)
(* :Date: 2021-07-19 *)

(* :Package Version: 0.5 *)
(* :Mathematica Version: 12.3 *)
(* :Copyright: (c) 2021 Anton Antonov *)
(* :Keywords: *)
(* :Discussion:

## Problem formulation

We want to have a system that:

- Generates relevant, correct, executable programming code based natural language specifications of computational workflows

- Can automatically recognize the workflow types

- Can generate code for different programming languages and related software packages

The points above are given in order of importance; the most important are placed first.

## Examples

Consider the following -- intentionally short and non-specific -- computational workflow specifications:

```mathematica
lsCommands = {
   "Create a random dataset.",
   "Do quantile regression over findData.",
   "Make a classifier over dsTitanic."};
```

Here we generate the code -- note the only the list of commands is given to the function Concretize, [AAp1]:

```mathematica
aRes = Concretize[lsCommands];
```

Here we tabulate the code generation (templates fill-in) results:

```mathematica
ResourceFunction["GridTableForm"][List @@@ Normal[aRes], TableHeadings -> {"Spec", "Code"}]
```

## References

[AAp1] Anton Antonov,
Computational workflow type classifier Mathematica package,
(2021),
[NLP-Template-Engine at GitHub/antononcube](https://github.com/antononcube/NLP-Template-Engine).

*)



(***********************************************************)
(* Package definition                                      *)
(***********************************************************)

BeginPackage["AntonAntonov`NLPTemplateEngine`NLPTemplateEngineCore`"];
(* Exported symbols added here with SymbolName::usage *)

(*
GetRawAnswers::usage = "GetRawAnswers[wfSpec, spec, opts] \
finds probability-scored answers of parameters questions for the computational workflow template wfSpec \
over the computational specification spec.";

GetAnswers::usage = "GetAnswers[wfSpec, spec, opts] \
finds precise answers of parameters questions for the computational workflow template wfSpec \
over the computational specification spec.";

Concretize::usage = "Concretize[ttype : (_String | Automatic), spec_String, opts] \
finds parameter values to fill-in the slots for the ttype template based on the natural language specification spec \
and creates corresponding executable expression. \
If ttype is Automatic then a dedicated classifier function used to guess the template type.";


NLPTemplateEngineAddData::usage = "NLPTemplateEngineAddData[dsSpecs_Dataset] \
adds the template engine data specifications dsSpecs into Concretize[\"Data\"].";

NLPTemplateEngineReplaceData::usage = "NLPTemplateEngineReplaceData[dsSpecs_Dataset] \
replaces the data in Concretize[\"Data\"] with template engine data specifications dsSpecs.";

ConvertCSVData::usage = "ConvertCSVData is an internal function.";

ConvertCSVDataForType::usage = "ConvertCSVDataForType is an internal function.";
*)

Begin["`Private`"];

Needs["AntonAntonov`NLPTemplateEngine`"];
Needs["AntonAntonov`NLPTemplateEngine`NLPTemplateEngineData`"];
Needs["AntonAntonov`NLPTemplateEngine`ComputationalWorkflowTypeClassifier`"];
Needs["AntonAntonov`NLPTemplateEngine`OpenAIFindTextualAnswer`"];

(***********************************************************)
(* GetRawAnswers                                           *)
(***********************************************************)

Clear[GetRawAnswers];

GetRawAnswers::nwft =
    "The first argument is an unknown workflow type. The first argument is expected to be one of `1`.";

Options[GetRawAnswers] = Join[
  {Method -> FindTextualAnswer},
  Options[FindTextualAnswer],
  Options[OpenAIFindTextualAnswer]
];

GetRawAnswers[workflowTypeArg_String, command_String, nAnswers_Integer : 4, opts : OptionsPattern[]] :=
    Block[{workflowType = workflowTypeArg, aShortcuts, aQuestions, mFunc, aRes},

      aShortcuts = Concretize["Data"]["Shortcuts"];
      aQuestions = Concretize["Data"]["Questions"];

      workflowType = workflowType /. aShortcuts;

      If[! MemberQ[Values[aShortcuts], workflowType],
        Message[GetRawAnswers::nwft, Union[Keys[aShortcuts], Values[aShortcuts]]];
        Return[$Failed]
      ];

      mFunc = OptionValue[GetRawAnswers, Method];
      If[TrueQ[mFunc === Automatic], mFunc = FindTextualAnswer];

      If[ TrueQ[mFunc === OpenAIFindTextualAnswer],
        aRes = Association @ OpenAIFindTextualAnswer[command, Keys@aQuestions[workflowType], "Rules" -> True, FilterRules[{opts}, Options[OpenAIFindTextualAnswer]]];
        aRes = Map[List @ Append[#, 0.99]&, aRes],
        (*ELSE*)
        aRes =
            Association@
                Map[# -> FindTextualAnswer[command, #, nAnswers, {"String", "Probability"}, FilterRules[{opts}, Options[FindTextualAnswer]]] &, Keys@aQuestions[workflowType]]
      ];

      Map[Association[Rule @@@ #] &, aRes]
    ];


(***********************************************************)
(* GetAnswers                                              *)
(***********************************************************)

ClearAll[TakeLargestKey];
TakeLargestKey[args__] := StringTrim@First@Keys@TakeLargest[args];

ClearAll[RemoveContextWords];
RemoveContextWords[s : (_String | {_String ..}), {} ] := s;
RemoveContextWords[s : (_String | {_String ..}), words : {_String..} ] :=
    StringTrim[StringReplace[ s, Thread[Thread[StringExpression[WordBoundary, words, WordBoundary]] -> ""], IgnoreCase -> True ]];
RemoveContextWords[s_, args___] := (Message[GetAnswers::rw];s);


Clear[GetAnswers];

GetAnswers::ncrw = "Problematic \"ContextWordsToRemove\" specification";

Options[GetAnswers] = Join[ Options[GetRawAnswers], {"RemoveByThreshold" -> True}];

GetAnswers[workflowTypeArg_String, command_String, nAnswers_Integer : 4, opts : OptionsPattern[]] :=
    Block[{workflowType = workflowTypeArg, aShortcuts, aQuestions, aRes, aParameterQuestions, parVal},

      aShortcuts = Concretize["Data"]["Shortcuts"];
      aQuestions = Concretize["Data"]["Questions"];

      (*
       We have multiple questions for each parameter in order to capture relevant answers
       from different types of phrasings.
       We hope that the (1) multiplicity of questions and (2) passing threshold and redundant words per question
       would suffice to get correct answers (most of the time.)
      *)

      (*"Normalize" the specified workflow type with replacement rules.*)
      workflowType = workflowType /. aShortcuts;

      (*Get raw answers.*)
      (*For each question we get an association of scored answers. The answers are keys. *)
      aRes = GetRawAnswers[workflowType, command, nAnswers, FilterRules[{opts}, Options[GetRawAnswers]]];
      If[TrueQ[aRes === $Failed],
        Return[$Failed]
      ];

      (*Filter out candidates with too low probabilities.*)
      (*For each question apply the threshold filtering.*)
      If[ TrueQ[OptionValue[GetAnswers, "RemoveByThreshold"]],
        aRes =
            Association@
                KeyValueMap[
                  Function[{k, v},
                    k -> Select[v, # >= aQuestions[workflowType, k, "Threshold"] &]], aRes];
        aRes = Select[aRes, Length[#] > 0 &];
      ];

      (*Remove specified "redundant" words.*)
      (*For each question and each key/answer replace redundant words with empty string.*)
      aRes =
          Association@
              KeyValueMap[
                Function[{k, v},
                  k ->
                      KeySelect[
                        KeyMap[RemoveContextWords[#, Lookup[aQuestions[workflowType, k], "ContextWordsToRemove", {}]]&, v],
                        StringLength[#] > 0&
                      ]
                ], aRes];
      aRes = Select[aRes, Length[#] > 0 &];

      (*Group the questions per parameter.*)
      aParameterQuestions = GroupBy[aQuestions[workflowType], #["Parameter"] &, Keys];
      (*aParameterCandidateValues=Map[Merge[Values@KeyTake[aRes,#],Max]&, aParameterQuestions];*)

      (*For each parameter and each question of that parameter extract the top candidate parameter value and associated probability.*)
      aRes =
          Map[
            KeyValueMap[
              Function[{k, v},
                parVal =
                    Switch[
                      ToString[aQuestions[workflowType, k, "TypePattern"]],

                      "{_String..}",

                      Map["\"" <> # <> "\"" &,
                        Select[StringTrim[StringSplit[TakeLargestKey[v, 1], {",", "and"}]], StringLength[#] > 0 &]],

                      "{_?NumericQ..}" | "{_Integer..}",

                      ToExpression /@ Select[StringTrim[StringSplit[TakeLargestKey[v, 1], {",", "and"}]], StringLength[#] > 0&],

                      "_?NumericQ" | "_Integer" | "_Symbol",
                      ToExpression[First @ Select[StringTrim[StringSplit[TakeLargestKey[v, 1], {",", "and", Whitespace}]], StringLength[#] > 0&]],

                      "_?BooleanQ" | "(True|False)" | "(False|True)",
                      MemberQ[
                        If[KeyExistsQ[aQuestions[workflowType, k], "TrueValues"], aQuestions[workflowType, k, "TrueValues"], {"true", "false"}],
                        ToLowerCase[TakeLargestKey[v, 1]]
                      ],

                      _,
                      TakeLargestKey[v, 1]
                    ];
                parVal -> TakeLargest[v, 1][[1]]
              ],
              KeyTake[aRes, Flatten[{#}]]
            ] &,
            aParameterQuestions
          ];

      (*For each parameter take the parameter value candidate with the largest probability *)

      aRes = TakeLargestBy[#, #[[2]] &, 1][[1]] & /@ Select[aRes, Length[#] > 0 &];

      (*Drop the associated probabilities*)
      Keys /@ aRes
    ];


(***********************************************************)
(* Concretize                                              *)
(***********************************************************)

Clear[Concretize];

SyntaxInformation[Concretize] = {"ArgumentsPattern" -> {_, _., OptionsPattern[]}};

Options[Concretize] =
    Join[
      Options[GetAnswers],
      {"TargetLanguage" -> "WL",
        "AvoidMonads" -> False,
        "AssociationResult" -> False,
        "UserID" -> None,
        "CopyToClipboard" -> True }
    ];

Concretize::tlang = "The value of the option \"TargetLanguage\" is expected to be one of `1`.";
Concretize::aulang = "The automatic programming language detection failed. Continuing by using \"WL\".";
Concretize::nargs = "If one argument is given then the first argument is expected to be a string or a list of strings. \
If two arguments are given then the first argument is expected to be a classifier function or Automatic, \
and the second argument is expected to be a string or a list of strings.";

Concretize["Data"] := NLPTemplateEngineData["Standard"];

Concretize["Templates"] := Concretize["Data"]["Templates"];

Concretize["Questions"] := Concretize["Data"]["Questions"];

Concretize["Defaults"] := Concretize["Data"]["Defaults"];

Concretize["Shortcuts"] := Concretize["Data"]["Shortcuts"];

Concretize[ commands : ( _String | {_String..} ), opts : OptionsPattern[]] :=
    Concretize[Automatic, commands, opts];

Concretize[ sf : (Automatic | _ClassifierFunction | _String), commands : {_String..}, opts : OptionsPattern[]] :=
    Association @ Map[ # -> Concretize[sf, #, opts]&, commands];

Concretize[Automatic, command_String, opts : OptionsPattern[]] :=
    Block[{cf},

      cf = GetComputationalWorkflowTypeClassifier[];

      If[ TrueQ[OptionValue[Concretize, "AvoidMonads"]],
        Concretize[ cf[command], command, opts],
        (*ELSE*)
        Concretize[ cf[command] /. {"Classification" -> "ClCon", "QuantileRegression" -> "QRMon"}, command, opts]
      ]
    ];

Concretize[cf_ClassifierFunction, command_String, opts : OptionsPattern[]] :=
    Concretize[ cf[command], command, opts];

Concretize[workflowTypeArg_String, command_String, opts : OptionsPattern[]] :=
    Block[{workflowType = workflowTypeArg,
      aQuestions, aTemplates, aDefaults, aShortcuts,
      lsKnownLanguages, userID, lang, cpcbQ, aRes, code, codeExpr, res},

      {aQuestions, aTemplates, aDefaults, aShortcuts} = Values @ KeyTake[ Concretize["Data"], {"Questions", "Templates", "Defaults", "Shortcuts"}];

      lsKnownLanguages = Union @ Flatten @ Values[ Keys /@ aTemplates ];

      (* User ID *)
      userID = OptionValue[Concretize, "UserID"];
      If[ MemberQ[{None, Automatic}, userID], userID = ""];
      userID = ToString[userID];

      (* Programming language *)
      lang = OptionValue[Concretize, "TargetLanguage"];
      If[ TrueQ[lang === Automatic],
        aRes = Join[aDefaults["ProgrammingEnvironment"], GetAnswers["ProgrammingEnvironment", command]];
        lang =
            Which[
              Length[StringCases[ aRes["Language"], WordBoundary ~~ ("WL" | "Mathematica") ~~ WordBoundary]] > 0, "WL",
              Length[StringCases[ aRes["Language"], WordBoundary ~~ ("R") ~~ WordBoundary]] > 0, "R",
              True,
              Message[Concretize::aulang];
              "WL"
            ]
      ];

      If[ TrueQ[StringQ[lang] && ToLowerCase[lang] == "mathematica"], lang = "WL"];

      If[ !StringQ[lang] || !MemberQ[ ToUpperCase[lsKnownLanguages], ToUpperCase[lang] ],
        Message[Concretize::tlang, Append[lsKnownLanguages, Automatic]];
        lang = "WL"
      ];

      (*CopyToClipboard*)
      cpcbQ = TrueQ[OptionValue[Concretize, "CopyToClipboard"]];

      (*Translation*)
      workflowType = workflowType /. aShortcuts;

      aRes = GetAnswers[workflowType, command, FilterRules[{opts}, Options[GetAnswers]]];

      If[TrueQ[aRes === $Failed],
        Return[$Failed]
      ];

      code = Concretize["Templates"][workflowType][lang][Join[aDefaults[workflowType], aRes]];

      Which[
        lang == "WL",
        codeExpr = ToExpression["Hold[" <> code <> "]"],

        lang == "R",
        code =
            StringReplace[
              code,
              {
                WordBoundary ~~ "Automatic" ~~ WordBoundary -> "NULL",
                WordBoundary ~~ "True" ~~ WordBoundary -> "TRUE",
                WordBoundary ~~ "False" ~~ WordBoundary -> "FALSE",
                "{" ~~ x : (Except[Characters["{}"]]..) ~~ "}" :> "c(" <> x <> ")"
              }];
        codeExpr = "parse( text = '" <> code <> "')",

        lang == "Python",
        code =
            StringReplace[
              code,
              {
                WordBoundary ~~ "Automatic" ~~ WordBoundary -> "None",
                "{" ~~ x : (Except[Characters["{}"]]..) ~~ "}" :> "[" <> x <> "]"
              }];
        codeExpr = code,

        lang == "Raku",
        code =
            StringReplace[
              code,
              {
                WordBoundary ~~ "Automatic" ~~ WordBoundary -> "Whatever",
                "{" ~~ x : (Except[Characters["{}"]]..) ~~ "}" :> "[" <> x <> "]"
              }];
        codeExpr = code,

        True,
        codeExpr = code
      ];

      res =
          If[ TrueQ[OptionValue[Concretize, "AssociationResult"]],
            <|
              "CODE" -> code,
              "USERID" -> userID,
              "DSLTARGET" -> lang <> "::" <> workflowType,
              "DSL" -> workflowType,
              "DSLFUNCTION" -> With[{wf = workflowType, l = lang, am = TrueQ[OptionValue[Concretize, "AvoidMonads"]]},
                ToString[Concretize[wf, #,
                  "TargetLanguage" -> l,
                  "AvoidMonads" -> am,
                  "AssociationResult" -> True]&]
              ]
            |>,
            (*ELSE*)
            codeExpr
          ];

      If[cpcbQ, CopyToClipboard[res]];

      res
    ];

Concretize[___] :=
    Block[{},
      Message[Concretize::nargs];
      $Failed
    ];


(***********************************************************)
(* Introspection data preparation functions                *)
(***********************************************************)

Clear[ToWorkflowTemplate];
ToWorkflowTemplate[spec_String] := Concretize["Templates"][spec]["WL"][Concretize["Defaults"][spec]];
ToWorkflowTemplate[x_] := x;

Clear[SplitWorkflowType];
SplitWorkflowType[name_String] := StringRiffle[StringCases[name, CharacterRange["A", "Z"] ~~ (CharacterRange["a", "z"] ..)]];
SplitWorkflowType[name_String] := StringRiffle[{StringReplace[name, "Mon" -> ""], "monad"}] /; StringMatchQ[name, __ ~~ "Mon"];
SplitWorkflowType["ClCon"] := "Classification monad";


(***********************************************************)
(* Introspection data                                      *)
(***********************************************************)

lsWorkflowTypes = Keys[Concretize["Templates"]];
txtWorkflowTypes = StringRiffle[lsWorkflowTypes, ", "];
txtSQASDescription1 = "The current specialized Question Answering System (QAS) has the workflow types " <> txtWorkflowTypes <> ".";
txtSQASDescription1 =
    txtSQASDescription1 <> "\nThe known workflow types are " <> txtWorkflowTypes <> ".";
txtSQASDescription1 =
    txtSQASDescription1 <> "\nThe implemented workflow template types are " <> txtWorkflowTypes <> ".";
txtSQASDescription1 =
    txtSQASDescription1 <> "\nThe number of implemented workflow templates is " <> ToString[Length@lsWorkflowTypes] <> ".";
txtSQASDescription1 =
    txtSQASDescription1 <> "\nThere are " <> ToString[Length@lsWorkflowTypes] <> " workflows.";
txtSQASDescription1 =
    txtSQASDescription1 <> "\nThere are " <> ToString[Length@lsWorkflowTypes] <> " templates per programming language.";

txtSQASDescription2 =
    Map["The template for " <> # <> " is: WorkflowTemplate-" <> # <> "." &, Keys[Concretize["Templates"]]];

txtSQASDescription2 =
    Join[
      txtSQASDescription2,
      Map["The template for " <> SplitWorkflowType[#] <> " is: WorkflowTemplate-" <> # <> "." &, Keys[Concretize["Templates"]]]
    ];

txtSQASDescription2 =
    Join[
      txtSQASDescription2,
      Map["The " <> SplitWorkflowType[#] <> " workflow template is: WorkflowTemplate-" <> # <> "." &, Keys[Concretize["Templates"]]]
    ];

txtSQASDescription2 =
    Join[
      txtSQASDescription2,
      StringReplace[Select[txtSQASDescription2, Length[StringCases[#, "LatentSemanticAnalysis"]] > 0 &], WhitespaceCharacter ~~ "LatentSemanticAnalysis" ~~ WhitespaceCharacter -> " LSAMon "],
      StringReplace[Select[txtSQASDescription2, Length[StringCases[#, "Recommendations"]] > 0 &], WhitespaceCharacter ~~ "Recommendations" ~~ WhitespaceCharacter -> " SMRMon "]
    ];

txtSQASDescription2 = StringRiffle[Union[txtSQASDescription2], "\n"];

txtSQASDescription = StringJoin[txtSQASDescription1, "\n", txtSQASDescription2];


(***********************************************************)
(* Concretize continued                                    *)
(***********************************************************)

Concretize["Introspection", query_String, opts : OptionsPattern[]] :=
    Block[{res},
      res = FindTextualAnswer[txtSQASDescription, query, FilterRules[{opts}, Options[FindTextualAnswer]]];
      res =
          StringReplace[
            res,
            "WorkflowTemplate-" ~~ (x : (LetterCharacter..)) ~~ WordBoundary :> Concretize["Templates"][x]["WL"][Concretize["Defaults"][x]]
          ];
      res
    ];


(***********************************************************)
(* Convert CSV data into associations                      *)
(***********************************************************)

Clear[ConvertCSVData];

ConvertCSVData::cnames = "The first argument is expected to be a dataset with column names `1`.";

ConvertCSVData[ dsTESpecs_Dataset ] :=
    Block[{lsExpectedColumnNames},

      (* Check correctness of the dataset. *)
      lsExpectedColumnNames = {"DataType", "WorkflowType", "Group", "Key", "Value"};
      If[ Length[Intersection[Normal@Keys@First@dsTESpecs, lsExpectedColumnNames]] < Length[lsExpectedColumnNames],
        Message[ConvertCSVData::cnames, ToString[lsExpectedColumnNames]];
        Return[$Failed]
      ];

      Association @ Map[ # -> ConvertCSVDataForType[ dsTESpecs, #]&, {"Questions", "Templates", "Defaults", "Shortcuts"}]
    ];


Clear[ConvertCSVDataForType];

ConvertCSVDataForType[ dsTESpecs_Dataset, dataType : "Questions" ] :=
    Block[{dsQuery, aRes},

      dsQuery = dsTESpecs[Select[#DataType == dataType&]];
      If[ Length[dsQuery] == 0, Return[<||>]];

      dsQuery = dsQuery[ All, If[ #Key == "ContextWordsToRemove", Append[#, "Value" -> ToExpression[#Value]], #]&];
      dsQuery = Normal[dsQuery[Values]];
      aRes = ResourceFunction["AssociationKeyDeflatten"][ Map[ Most[#] -> Last[#]&, dsQuery] ];

      aRes[dataType]
    ];

ConvertCSVDataForType[ dsTESpecs_Dataset, dataType : "Templates" ] :=
    Block[{dsQuery, aRes},

      dsQuery = dsTESpecs[Select[#DataType == dataType&]];
      If[ Length[dsQuery] == 0, Return[<||>]];

      (* We drop the "Key" column that has to have "Template" value,
      since that column was added to fit the global long-format CSV. *)
      dsQuery = dsQuery[ All, KeyDrop[#, "Key"]&];
      dsQuery = Normal[dsQuery[Values]];

      aRes = ResourceFunction["AssociationKeyDeflatten"][ Map[ Most[#] -> If[ StringQ[Last[#]], ToExpression[Last[#]], Last[#]]&, dsQuery] ];

      aRes[dataType]
    ];

ConvertCSVDataForType[ dsTESpecs_Dataset, dataType : "Templates" ] :=
    Block[{dsQuery, ToTemplateExpression, aRes},

      dsQuery = dsTESpecs[Select[#DataType == dataType&]];
      If[ Length[dsQuery] == 0, Return[<||>]];

      (* We drop the "Key" column that has to have "Template" value,
      since that column was added to fit the global long-format CSV. *)
      dsQuery = dsQuery[ All, KeyDrop[#, "Key"]&];
      dsQuery = Normal[dsQuery[Values]];

      ToTemplateExpression = If[ StringMatchQ[ #, StartOfString ~~ "TemplateObject[" ~~ __], ToExpression[#], StringTemplate[#]]&;
      aRes = ResourceFunction["AssociationKeyDeflatten"][ Map[ Most[#] -> ToTemplateExpression[Last[#]] &, dsQuery] ];

      aRes[dataType]
    ];

ConvertCSVDataForType[ dsTESpecs_Dataset, dataType : "Defaults" ] :=
    Block[{dsQuery, aRes},

      dsQuery = dsTESpecs[Select[#DataType == dataType&]];
      If[ Length[dsQuery] == 0, Return[<||>]];

      (* We drop the "Group" column that has to have "All" value,
      since that column was added to fit the global long-format CSV. *)
      (* Meaning, currently the defaults depend only from the workflow type,
       not the workflow type and the target language.*)
      dsQuery = dsQuery[ All, KeyDrop[#, "Group"]&];
      dsQuery = Normal[dsQuery[Values]];

      aRes = ResourceFunction["AssociationKeyDeflatten"][ Map[ Most[#] -> ToExpression[Last[#]]&, dsQuery] ];

      aRes[dataType]
    ];

ConvertCSVDataForType[ dsTESpecs_Dataset, dataType : "Shortcuts" ] :=
    Block[{dsQuery},

      dsQuery = dsTESpecs[Select[#DataType == dataType&]];
      If[ Length[dsQuery] == 0, Return[<||>]];

      (* We only need "Key" and "Value" for the shortcuts. *)
      dsQuery = dsQuery[ All, {"Key", "Value"}];

      Normal @ dsQuery[ Association, #Key -> #Value& ]
    ];


(***********************************************************)
(* Add template engine data                                *)
(***********************************************************)

Clear[NLPTemplateEngineAddData];

NLPTemplateEngineAddData::nargs = "The first argument is expected to be a dataset.";

NLPTemplateEngineAddData[ dsTESpecs_Dataset ] :=
    Block[{aRes},

      aRes = ConvertCSVData[dsTESpecs];

      Map[
        With[{k = #, ascGlobal = Concretize["Data"],
          asc = Concretize["Data"][#], newAsc = aRes[#]},
          Concretize["Data"] := Append[ascGlobal, k -> Join[asc, newAsc]]
        ] &,
        Keys[aRes]
      ];

      Concretize["Data"]
    ];

NLPTemplateEngineAddData[___] :=
    Block[{},
      Message[NLPTemplateEngineAddData::nargs];
      $Failed
    ];


(***********************************************************)
(* Replace template engine data                            *)
(***********************************************************)

Clear[NLPTemplateEngineReplaceData];

NLPTemplateEngineReplaceData::nargs = "The first argument is expected to be a dataset.";

NLPTemplateEngineReplaceData[ dsTESpecs_Dataset ] :=
    Block[{aRes},

      aRes = ConvertCSVData[dsTESpecs];
      (* Some verification is would be nice. *)

      Concretize["Data"] = aRes
    ];

NLPTemplateEngineReplaceData[___] :=
    Block[{},
      Message[NLPTemplateEngineReplaceData::nargs];
      $Failed
    ];


End[];

EndPackage[];