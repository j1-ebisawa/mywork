echo off
set /P IN="Input jcl name->"
iscrun AJ_ISRUN -c run.cfg AJ_JCL jcl\%IN%.jcl >%IN%.log
