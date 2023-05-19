@ECHO OFF

reg query "HKLM\Software\Microsoft\Internet Explorer" /v svcVersion /reg:32 2> NUL
reg query "HKLM\Software\Microsoft\Internet Explorer" /v version /reg:32 2> NUL
