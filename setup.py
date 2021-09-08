# This program is a part of EASIFEM library.
# See. www.easifem.com
# Copyright (c) 2020-2021, All right reserved, Vikas Sharma, Ph.D.
#

import os
import sys
import platform

str=" _______     ___           _______. __   _______  _______ .___  ___. "
print(str)
str="|   ____|   /   \         /       ||  | |   ____||   ____||   \/   | "
print(str)
str="|  |__     /  ^  \       |   (----`|  | |  |__   |  |__   |  \  /  | "
print(str)
str="|   __|   /  /_\  \       \   \    |  | |   __|  |   __|  |  |\/|  | "
print(str)
str="|  |____ /  _____  \  .----)   |   |  | |  |     |  |____ |  |  |  |"
print(str)
str="|_______/__/     \__\ |_______/    |__| |__|     |_______||__|  |__|"
print(str+"\n")
str = "Expandable And Scalable Infrastrcture for Finite Element Methods"
print(str)
str = "Developed by Vikas Sharma, Ph. D."
print(str)
str = "(c) 2020-present"
print(str)
print("")
print("================================================================\n")

def installpkgs():
    while True:
        choice = input( f"Do you want to automatically Install external packages? 'yes' or 'no' [Y/n]: " ).lower()
        if choice in ['Y', 'y', 'ye', 'yes']:
            return True
        else:
            return False

def setEnvVar():
    while True:
        choice = input( f"Do you want to automatically set environment variables? 'yes' or 'no' [Y/n]: " ).lower()
        if choice in ['Y', 'y', 'ye', 'yes']:
            return True
        else:
            return False

print("Detecting OS type...")
_os = platform.system()
if _os == 'Windows':
    print("Windows platform found")
    print("Setting up for Windows...")
    print("ERROR: INSTALLATION on windows is work in progress")
    exit
    #os.system("install.bat")
    print("Please use Windows Subsystem Linux(WSL) ")
    print("Installation DONE!!")

elif _os == "Darwin":
    print("MacOSX system found")
    print("Setting up for MacOSX...")
    if( installpkgs() ):
        os.system( "sh ./setup/install_pkgs_Darwin.sh" )
    if( setEnvVar() ):
        os.system("sh ./setup/set_envvar_Darwin.sh")
    print("Installation DONE!!")

elif _os == "Linux":
    print("Linux system found")
    print("Setting up for Linux...")
    if(installpkgs()):
        os.system("${SHELL} ./setup/install_pkgs_Ubuntu.sh")
    if(setEnvVar()):
        os.system("${SHELL} ./setup/set_envvar_Ubuntu.sh")

else:
    print("ERROR: Unknown Operating System")
