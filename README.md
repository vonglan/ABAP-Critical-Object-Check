# ABAP-Critical-Object-Check
ABAP Package to check transports whether they might lead to dumps during import

## Prerequisites
The program is tested on a Basis 7.50 system.

I do not know whether it also works on an S/4 system, or on older releases.

The "dump mechanisms" might be different.

## Installation

### Install package with abapGit
See documentation here: https://docs.abapgit.org/

The easiest way is to use the standalone version of abapGit, and the offline mode (no direct connection to github, but transfer with ZIP file).

### Optionally, add logger class
The output uses the application log (SBAL), and a classical list for the details.

The current implementation expects the great logger class (SBAL wrapper) from here: https://github.com/ABAP-Logger/ABAP-Logger

The name of the logger class (in our systems we changed it to ZCL_X_LOGGER_FACTORY etc. due to naming conventions) needs to be passed to the constant con_logger_name in class zcl_s_trkorr_chk_critical_obj.

The logger is called dynamically (to avoid syntax errors if the logger name can not be found in your installation). You can change that to normal (static) calls for your logger class, especially if your logger methods have different names/syntax.

If the logger class is not found in the system, as fallback, the output is at the bottom of the classical list.

### If you use the logger, create or adapt log object
Using transaction SLG0, define log oject ZS_DEV and sub object TRKORR_CRIT, or replace the log object names in the coding (class zcl_s_trkorr_chk_critical_obj).

### Optionally, call from transport release exit
If you want the check to be executed for each transport release, use the transport release BAdI.

In method METHOD if_ex_cts_request_check~check_before_release, you need to call the check class like this:

```
zcl_s_trkorr_chk_critical_obj=>check( i_it_trkorr = value #( ( request ) ) ).
zcl_s_trkorr_chk_critical_obj=>display_findings( ).
```
 
This works only with logger installed.

Also adapt the long text for message ZS_DEV_CRIT_OBJ 012 according to your company's policy.

## Optionally, configure critical objects
In table ZSTR_CRIT_OBJS. We currently have some Z objects and the following ERP objects:


```
R3TR	FUGR	MBMB
R3TR	FUGR	MBWL
R3TR	FUGR	MEGUI
R3TR	FUGR	MEPO
R3TR	FUGR	MEPOBADI
R3TR	FUGR	MIGO
R3TR	FUGR	V50S
R3TR	FUGR	V61A
R3TR	FUGR	V61B
R3TR	PROG	SAPMM07M
R3TR	PROG	SAPMM61R
R3TR	PROG	SAPMV45A
R3TR	PROG	SAPMV50A
R3TR	PROG	SAPMV60A
```

## Usage
Report zs_test_crit_obj (which also provides a classical list with the dependency hierarchy, after you leave the application log).

Or call the class from the transport release BAdI, see above.

## Disclaimer
Use at your own risk.

In general, this is an automatic check. It can provide false positives as well as false negatives. Do not switch off your own brain, experience etc.

