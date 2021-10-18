# COVIDTrtTrialDesign


Steps for updating (bash/cmd line):
- update code
- rebuild docker
  - docker ps -a
  - docker build -t seqtrialdesign:latest .
  - docker stop seqtrialdesign; docker rm seqtrialdesign
- add, commit, push

To run locally:
 - docker run -d --name seqtrialdesign -p 8888:8888 seqtrialdesign:latest
 - open your browser and go to localhost:8888
