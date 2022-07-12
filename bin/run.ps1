.\bin\giper_1.exe
New-Item -ItemType Directory -Force -Path output
Move-Item -Force -Path *.txt, *.xls -Exclude requirements.txt -Destination output