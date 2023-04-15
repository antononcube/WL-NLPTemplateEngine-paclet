(* ::Package:: *)

(* ::Section:: *)
(*Package Header*)


BeginPackage["AntonAntonov`NLPTemplateEngine`"];


(* ::Text:: *)
(*Declare your public symbols here:*)


GetRawAnswers;
GetAnswers;
Concretize;
NLPTemplateEngineAddData;
NLPTemplateEngineReplaceData;
ConvertCSVData;
ConvertCSVDataForType;
OpenAIFindTextualAnswer;

Begin["`Private`"];


(* ::Section:: *)
(*Definitions*)


(* ::Text:: *)
(*Define your public and private symbols here:*)

Needs["AntonAntonov`NLPTemplateEngine`NLPTemplateEngineData`"];
Needs["AntonAntonov`NLPTemplateEngine`ComputationalWorkflowTypeClassifier`"];
Needs["AntonAntonov`NLPTemplateEngine`NLPTemplateEngineCore`"];
Needs["AntonAntonov`NLPTemplateEngine`OpenAIFindTextualAnswer`"];

(* ::Section::Closed:: *)
(*Package Footer*)


End[];
EndPackage[];