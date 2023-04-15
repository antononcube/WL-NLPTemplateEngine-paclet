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
ChatCompletionModelQ::usage = "Checks if a given string an identifier of a chat completion model.";
TextCompletionModelQ::usage = "Checks if a given string an identifier of a text completion model.";

Begin["`Private`"];

Needs["AntonAntonov`NLPTemplateEngine`"];
Needs["ChristopherWolfram`OpenAILink`"];

(***********************************************************)
(* End-points and models correspondence                    *)
(***********************************************************)

(* Taken from: https://platform.openai.com/docs/models/model-endpoint-compatibility *)
aEndPointToModels =
    <|"/v1/moderations" -> {"text-moderation-stable", "text-moderation-latest"},
      "/v1/edits" -> {"text-davinci-edit-001", "code-davinci-edit-001"},
      "/v1/chat/completions" -> {"gpt-4", "gpt-4-0314", "gpt-4-32k", "gpt-4-32k-0314", "gpt-3.5-turbo", "gpt-3.5-turbo-0301"},
      "/v1/audio/transcriptions" -> "whisper-1",
      "/v1/audio/translations" -> "whisper-1",
      "/v1/completions" -> {"text-davinci-003", "text-davinci-002", "text-curie-001", "text-babbage-001", "text-ada-001"},
      "/v1/fine-tunes" -> {"davinci", "curie", "babbage", "ada"},
      "/v1/embeddings" -> {"text-embedding-ada-002", "text-search-ada-doc-001"}|>;

aModelToEndPoints =
    <|"gpt-3.5-turbo-0301" -> {"/v1/chat/completions"},
      "text-davinci-edit-001" -> {"/v1/edits"},
      "gpt-4-32k" -> {"/v1/chat/completions"},
      "curie" -> {"/v1/fine-tunes"},
      "text-davinci-002" -> {"/v1/completions"},
      "gpt-4" -> {"/v1/chat/completions"},
      "text-search-ada-doc-001" -> {"/v1/embeddings"},
      "text-curie-001" -> {"/v1/completions"},
      "davinci" -> {"/v1/fine-tunes"},
      "gpt-3.5-turbo" -> {"/v1/chat/completions"},
      "text-babbage-001" -> {"/v1/completions"},
      "text-embedding-ada-002" -> {"/v1/embeddings"},
      "ada" -> {"/v1/fine-tunes"},
      "whisper-1" -> {"/v1/audio/transcriptions", "/v1/audio/translations"},
      "babbage" -> {"/v1/fine-tunes"},
      "gpt-4-0314" -> {"/v1/chat/completions"},
      "text-davinci-003" -> {"/v1/completions"},
      "code-davinci-edit-001" -> {"/v1/edits"},
      "text-moderation-latest" -> {"/v1/moderations"},
      "text-ada-001" -> {"/v1/completions"},
      "text-moderation-stable" -> {"/v1/moderations"},
      "gpt-4-32k-0314" -> {"/v1/chat/completions"}|>;

(*---------------------------------------------------------*)
Clear[ChatCompletionModelQ];
ChatCompletionModelQ[m_String] := MemberQ[aEndPointToModels["/v1/chat/completions"], m];

(*---------------------------------------------------------*)
Clear[TextCompletionModelQ];
TextCompletionModelQ[m_String] := MemberQ[aEndPointToModels["/v1/completions"], m];


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
      Union[Options[OpenAITextComplete], Options[OpenAIChatComplete]]
    ];

OpenAIFindTextualAnswer[text_String, question_String, opts : OptionsPattern[]] :=
    OpenAIFindTextualAnswer[text, {question}, opts];

OpenAIFindTextualAnswer[text_String, questions_List, opts : OptionsPattern[]] :=
    Module[{model, sep, prelude, echoQ, rulesQ, request, query, msgObj, res, answers},

      (*-------------------------------------------------*)
      (* Process model                                   *)
      (*-------------------------------------------------*)
      model = OptionValue[OpenAIFindTextualAnswer, OpenAIModel];
      If[TrueQ[model === Automatic], model = "gpt-3.5-turbo" ];

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
      If[ ChatCompletionModelQ[model],
        msgObj = OpenAIChatMessageObject[<|"Role" -> "user", "Text" -> query|>];
        res = OpenAIChatComplete[msgObj, OpenAIModel -> model, FilterRules[{opts}, Options[OpenAIChatComplete]]];
        res = res["Text"],
        (*ELSE*)
        res = OpenAITextComplete[query, OpenAIModel -> model, FilterRules[{opts}, Options[OpenAITextComplete]]]
      ];

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
        Message[OpenAIFindTextualAnswer::nans];
        res
      ]
    ];



End[];

EndPackage[];