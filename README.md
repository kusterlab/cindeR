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
  - [Installation on linux](https://docs.docker.com/install/linux/docker-ce/ubuntu/)
  - [Installation on Windows 10](https://www.docker.com/get-started) 
  - [Installation on MacOS](https://www.docker.com/get-started)

  since June 20 you have to register to be able to download docker for Windows and MacOS.
  

### Execution
*Hint*: The first installation of the programm takes up to 15 minutes.
#### Linux

You need `sudo` rights if your user is not part of the group `docker` 
```
git clone https://github.com/kusterlab/cindeR.git
make run
```

#### Windows
1. Open `PowerShell`
2. `docker build https://github.com/kusterlab/cindeR -t cinder`
3. `docker run -p 7678:7678 cinder`
```

Step 2 is not necessary after the first run.
*Hint*: docker on Windows reacts with huge delays.

#### MacOS
You can follow the installation for Linux
The server/computer should now run a CindeR instance, which is available on port `7678`.
