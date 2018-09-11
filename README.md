# CindeR

Minimal web application to allow a quick judgement of any kind of dataset. 

## Usage
1. Upload data
2. Judge data to be either a positive or negative case
    - Positive samples: press → **or** swipe plot to the right
    - Negative samples: press ← **or** swipe plot to the left
3. Download judged data for further processing (e.g. [CurveClassification](https://github.com/kusterlab/curveClassification_shiny)) at any given timepoint (judging of your data can be resumed)

A more in detail description can be found in the provided [PDF](https://github.com/kusterlab/cindeR/raw/master/manual/cindeR_manual.pdf), but we also provide a video tutorial on [Youtube](https://youtu.be/xwYTkmQfzxY)

## Building your own CindeR instance

### Requirenment
- [Docker](https://www.docker.com/)

### Execution
```
make run
```
The server/computer should now run a CindeR instance, which is available on port `7678`.
