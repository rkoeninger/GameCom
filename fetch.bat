set SDL_VERSION=2.0.3
set FILE_NAME=SDL2-%SDL_VERSION%-win32-x64.zip
powershell.exe -Command "Invoke-WebRequest -Uri https://www.libsdl.org/release/%FILE_NAME% -OutFile %FILE_NAME%"
powershell.exe -Command "Expand-Archive %FILE_NAME% -DestinationPath libsdl2 -Force"
del %FILE_NAME%
