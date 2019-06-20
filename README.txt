AQA Installation and development notes

Create directory C:\Program Files\AQA

Copy installation files to C:\Program Files\AQA , which should create a directory something like 

    C:\Program Files\AQA\AQA-0.0.1

This software is written in Scala and uses YAJSW to wrap the main program in a Windows service.

Change directory to:

    C:\Program Files\AQA\AQA-0.0.1\yajsw-stable-11.03\bat

and as administrator run the installService.bat file

The service may be started by either running startService.bat or by by using Windows 'Services' GUI.

