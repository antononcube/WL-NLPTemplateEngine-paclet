(* Created by the Wolfram Language Plugin for IntelliJ, see http://wlplugin.halirutan.de/ *)

(* :Title: OpenAIFindTextualAnswer *)
(* :Context: OpenAIFindTextualAnswer` *)
(* :Author: Anton Antonov *)
(* :Date: 2023-04-15 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: 13.0+ *)
(* :Copyright: (c) 2023 Anton Antonov *)
(* :Keywords: *)
(* :Discussion:

*)



(***********************************************************)
(* Package definition                                      *)
(***********************************************************)

BeginPackage["AntonAntonov`NLPTemplateEngine`OpenAIFindTextualAnswer`"];
(* Exported symbols added here with SymbolName::usage *)

(*OpenAIFindTextualAnswer::usage = "OpenAIFindTextualAnswer[text, question, nAnswers, properties, opts___]";*)

Begin["`Private`"];

Needs["AntonAntonov`NLPTemplateEngine`"];
Needs["ChristopherWolfram`OpenAILink`"];

(***********************************************************)
(* OpenAIFindTextualAnswer                                 *)
(***********************************************************)

Clear[OpenAIFindTextualAnswer];

OpenAIFindTextualAnswer::nprld = "The value of the option \"Prelude\" is expected to be a string or Automatic.";
OpenAIFindTextualAnswer::nreq = "The value of the option \"Request\" is expected to be a string or Automatic.";
OpenAIFindTextualAnswer::nans = "The obtained answer does not have the expected form: a line with an answer for each question.";

Options[OpenAIFindTextualAnswer] =
    Join[
      {"Prelude" -> Automatic, "Request" -> Automatic, "Separator" -> Automatic, "StripWith" -> Automatic, "Rules" -> False, "Echo" -> False},
      Options[OpenAITextComplete]
    ];

OpenAIFindTextualAnswer[text_String, question_String, opts : OptionsPattern[]] :=
    OpenAIFindTextualAnswer[text, {question}, opts];

OpenAIFindTextualAnswer[text_String, questions_List, opts : OptionsPattern[]] :=
    Module[{sep, prelude, echoQ, rulesQ, request, query, res, answers},

      (*-------------------------------------------------*)
      (* Process separator                               *)
      (*-------------------------------------------------*)

      sep = OptionValue[OpenAIFindTextualAnswer, "Separator"];
      If[ TrueQ[sep === Automatic], sep = ")"];

      (*-------------------------------------------------*)
      (* Process prelude                                 *)
      (*-------------------------------------------------*)

      prelude = OptionValue[OpenAIFindTextualAnswer, "Prelude"];
      If[ TrueQ[prelude === Automatic], prelude = "Given the text:"];

      If[ !StringQ[prelude],
        Message[OpenAIFindTextualAnswer::nprld];
        Return[$Failed]
      ];

      (*-------------------------------------------------*)
      (* Process rules and echo                          *)
      (*-------------------------------------------------*)
      rulesQ = TrueQ[OptionValue[OpenAIFindTextualAnswer, "Rules"]];
      echoQ = TrueQ[OptionValue[OpenAIFindTextualAnswer, "Echo"]];

      (*-------------------------------------------------*)
      (* Process request                                 *)
      (*-------------------------------------------------*)

      request = OptionValue[OpenAIFindTextualAnswer, "Request"];
      If[ TrueQ[request === Automatic],
        request =
            If[ Length[questions] == 1,
              "Give the shortest answer of the question:",
              "List the shortest answers of the questions:"
            ];
      ];

      If[ !StringQ[request],
        Message[OpenAIFindTextualAnswer::nreq];
        Return[$Failed]
      ];

      (*-------------------------------------------------*)
      (* Make query                                      *)
      (*-------------------------------------------------*)

      query = prelude <> " \"" <> text <> "\" " <> request;

      query = Fold[ #1 <> "\n" <> ToString[#2] <> sep <> " " <> questions[[#2]] &, query, Range[Length[questions]]];

      If[ echoQ, Echo[Framed[query], "Query:"]];

      (* Delegate *)
      res = OpenAITextComplete[query, FilterRules[{opts}, Options[OpenAITextComplete]]];

      If[ echoQ, Echo[Framed[res], "Result:"]];

      (*-------------------------------------------------*)
      (* Post-process answers                            *)
      (*-------------------------------------------------*)

      (* Pick answers the are long enough. *)
      answers = Select[StringSplit[res, "\n"], StringLength[#] > StringLength[ToString[Length[questions]]] + StringLength[sep] + 1&];

      If[ echoQ, Echo[ColumnForm[answers], "Answers:"]];

      If[ Length[answers] == Length[questions],
        answers = Map[ StringTrim @ StringReplace[#, StartOfString ~~ DigitCharacter.. ~~ sep -> ""]&, answers];
        If[ rulesQ,
          Thread[questions -> answers],
          (*ELSE*)
          answers
        ]
        ,
        (* ELSE *)
        Message[]
            res
      ]

    ];



End[];

EndPackage[];