# NLPTemplateEngine WL paclet

## In brief

Wolfram Language (aka Mathematica) paclet that fills in (code) templates using NLP techniques.

The paclet has a template fill in function `Concretize` and a related classifier of natural language specifications.
A user can provide code templates to be filled in using CSV files.

The pre-loaded templates are used to produce Machine Learning (ML) computational workflow specifications for: 
- Classification
- Latent Semantic Analysis
- Quantile Regression
- Recommendations
- Random tabular data generation 


The implementations are in Python, R, and WL. Here is example of Python recommender creation spec:

-----

## Usage examples

### Classification (WL)

```mathematica
Concretize["Quantile regression with the dataset dfTemp; use 20 knots and the probabilities 0.1 and 0.9."]
```

```mathematica
Hold[qrObj = 
   QRMonUnit[dfTemp]⟹
     QRMonEchoDataSummary[]⟹
     QRMonQuantileRegression[20, {0.1, 0.9}, InterpolationOrder -> 3]⟹
     QRMonPlot["DateListPlot" -> False, PlotTheme -> "Detailed"]⟹
     QRMonErrorPlots["RelativeErrors" -> False, "DateListPlot" -> False, PlotTheme -> "Detailed"];
]
```

### Quantile Regression (WL)

```mathematica
Concretize["Quantile regression with the dataset dfTemp; use 20 knots and the probabilities 0.1 and 0.9."]
```

```mathematica
Hold[
 qrObj = QRMonUnit[dfTmep]⟹
     QRMonEchoDataSummary[]⟹
     QRMonQuantileRegression[20, {0.1, 0.9}, InterpolationOrder -> 3]⟹
     QRMonPlot["DateListPlot" -> False, PlotTheme -> "Detailed"]⟹
     QRMonErrorPlots["RelativeErrors" -> False, "DateListPlot" -> False, PlotTheme -> "Detailed"];
]
```


### Recommendations (Python)

```mathematica
Concretize["Make a recommender over the data dfMovies; Give recommendations for the profile actor:Willis and year:1995.", 
 "TargetLanguage" -> "Python"]
```

```python
smrObj = (SparseMatrixRecommender()
.create_from_wide_form(data = dfMovies, item_column_name="id", columns=None, add_tag_types_to_column_names=True, tag_value_separator=":")
.apply_term_weight_functions("IDF", "None", "Cosine")
.recommend_by_profile(profile=["Willis", "year"], nrecs=12)
.join_across(data=dfMovies, on="id")
.echo_value())
```

-----

## Workflow

*TBD...*

-----

## Targets

*TBF...*

### Classification

### Latent Semantic Analysis

### Quantile Regression

### Recommendations


### Random tabular dataset

- Python,  
- R,
- Raku
- WL, 
[`RandomTabularDataset`](https://resources.wolframcloud.com/FunctionRepository/resources/RandomTabularDataset/)



------

## Bring your own templates

0. Load the paclet [NLP-Template-Engine](https://resources.wolframcloud.com/PacletRepository/resources/AntonAntonov/NLPTemplateEngine/).
   

```mathematica
Needs["AntonAntonov`NLPTemplateEngine`"]
```

1. Get the "training" templates data (from CSV file you have created or changed) for a new workflow
   (["SendMail"](./TemplateData/dsQASParameters-SendMail.csv)):

```mathematica
dsSendMailTemplateEngineData = ResourceFunction["ImportCSVToDataset"][
  "https://raw.githubusercontent.com/antononcube/NLP-Template-Engine/main/TemplateData/dsQASParameters-SendMail.csv"];
Dimensions[dsSendMailTemplateEngineData]

(* {43, 5} *)
```

2. Add the ingested data for the new workflow (from the CSV file) into the NLP-Template-Engine:

```mathematica
NLPTemplateEngineAddData[dsSendMailTemplateEngineData] // Keys

(* {"Questions", "Templates", "Defaults", "Shortcuts"} *)
```

3. Parse natural language specification with the newly ingested and onboarded workflow ("SendMail"):

```mathematica
Concretize["SendMail", "Send email to joedoe@gmail.com with content RandomReal[343], and the subject this is a random real call.", PerformanceGoal -> "Speed"]

(* Hold[
 SendMail[
  Association["To" -> {"joedoe@gmail.com"}, 
   "Subject" -> "a random real call", "Body" -> RandomReal, 
   "AttachedFiles" -> None]]] *)
```

4. Experiment with running the generated code!

-----

## References

### Repositories

[AAr1] Anton Antonov
[NLP Template Engine](https://github.com/antononcube/NLP-Template-Engine),
(2021),
[GitHub/antononcube](https://github.com/antononcube).

### Videos

[AAv1] Anton Antonov
["NLP Template Engine, Part 1"](https://www.youtube.com/watch?v=a6PvmZnvF9I), 
(2021), 
[YouTube/AntonAntonov](https://www.youtube.com/@AAA4prediction).

[AAv2] Anton Antonov,
[Natural Language Processing Template Engine](https://www.youtube.com/watch?v=IrIW9dB5sRM), 
(2022), 
[Wolfram Technology Conference 2022](https://www.wolfram.com/events/technology-conference/2022/) 
presentation. 
[YouTube/WolframResearch](https://www.youtube.com/@WolframResearch).
