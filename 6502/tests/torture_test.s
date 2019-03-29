/* 16-bit code V1.33 */
_char_global
	DB(65)
_int_global
	DW(1)
_flt_global
	DB($81)
	DB($0)
	DB($0)
	DB($0)
	DB($0)
_dbl_global
	DB($81)
	DB($0)
	DB($0)
	DB($0)
	DB($0)
_struct_global
	DW(1)
	DW(3)
_tab_global
	DW(0)
	DW(1)
	DW(1)
	DW(2)
	DW(3)
	DW(5)
	ZERO(8)
_int_funct
	ENTER(0,6)
	ADDW_YCD((ap),0,3,tmp0)
	LEAVEW_D(tmp0)
_char_funct
	ENTER(0,6)
	CSBW_YD((ap),0,tmp0)
	ADDW_DCD(tmp0,3,tmp0)
	CSBW_DD(tmp0,tmp0)
	LEAVEW_D(tmp0)
_flt_funct
	ENTER(0,11)
	ADDF_YDY((ap),0,Ltorture_test130,(fp),6)
	LEAVEF_Y((fp),6)
_dbl_funct
	ENTER(0,11)
	ADDF_YDY((ap),0,Ltorture_test132,(fp),6)
	LEAVEF_Y((fp),6)
_struct_funct
	ENTER(0,12)
	MOVW_DY(op1,(fp),6)
	ADDW_YCD((ap),0+2,3,tmp0)
	ASGNW_DA(tmp0,(fp),8+2)
	ASGNW_DA(tmp0,(fp),8)
	ASGNS_AY((fp),8,(fp),6,4)
	LEAVE
_char_ptr_funct
	ENTER(0,6)
	ADDW_YCD((ap),0,3,tmp0)
	LEAVEW_D(tmp0)
_int_ptr_funct
	ENTER(0,6)
	ADDW_YCD((ap),0,6,tmp0)
	LEAVEW_D(tmp0)
_struct_ptr_funct
	ENTER(0,6)
	ADDW_YCD((ap),0,2,tmp0)
	ADDW_YCY((tmp0),0,3,(tmp0),0)
	MOVW_YD((ap),0,tmp0)
	LEAVEW_D(tmp0)
_char_ptr_global
	DW(_char_global)
_int_ptr_global
	DW(_tab_global+4+4)
_flt_ptr_global
	DW(_flt_global)
_struct_ptr_global
	DW(_struct_global)
_fct_ptr_global
	DW(_int_funct)
_check
	ENTER(0,6)
	NEW_YY((ap),0,(ap),2,Ltorture_test138)
	ARGW_C(Ltorture_test141,(sp),0)
	ARGW_Y((ap),4,(sp),2)
	CALLV_C(_printf,4)
	JUMP_C(Ltorture_test139)
Ltorture_test138
	ARGW_C(Ltorture_test143,(sp),0)
	ARGW_Y((ap),4,(sp),2)
	ARGW_Y((ap),0,(sp),4)
	ARGW_Y((ap),2,(sp),6)
	CALLV_C(_printf,8)
Ltorture_test139
	LEAVE
_simple_tests
	ENTER(0,24)
	ASGNF_DA(Ltorture_test145,(fp),14)
	ARGW_D(_int_global,(sp),0)
	ARGW_C(1,(sp),2)
	ARGW_C(Ltorture_test147,(sp),4)
	CALLV_C(_check,6)
	ARGW_D(_int_global,(sp),0)
	CALLW_CY(_int_funct,2,(fp),6)
	ARGW_Y((fp),6,(sp),0)
	ARGW_C(4,(sp),2)
	ARGW_C(Ltorture_test149,(sp),4)
	CALLV_C(_check,6)
	ARGW_D(_int_global,(sp),0)
	ARGW_C(1,(sp),2)
	ARGW_C(Ltorture_test149,(sp),4)
	CALLV_C(_check,6)
	ARGW_D(_int_global,(sp),0)
	CALLW_CD(_int_funct,2,_int_global)
	ARGW_D(_int_global,(sp),0)
	ARGW_C(4,(sp),2)
	ARGW_C(Ltorture_test151,(sp),4)
	CALLV_C(_check,6)
	CSBW_DD(_char_global,tmp0)
	ARGW_D(tmp0,(sp),0)
	CALLW_CY(_char_funct,2,(fp),8)
	CSBW_YD((fp),8,tmp0)
	ARGW_D(tmp0,(sp),0)
	ARGW_C(68,(sp),2)
	ARGW_C(Ltorture_test153,(sp),4)
	CALLV_C(_check,6)
	ARGF_D(_flt_global,(sp),0)
	CALLF_CY(_flt_funct,5,(fp),19)
	ASGNF_YA((fp),19,(fp),9)
	MOVF_YY((fp),9,(fp),19)
	CFI_YD((fp),19,tmp0)
	ARGW_D(tmp0,(sp),0)
	ARGW_C(4,(sp),2)
	ARGW_C(Ltorture_test155,(sp),4)
	CALLV_C(_check,6)
	ARGF_Y((fp),14,(sp),0)
	CALLF_CY(_dbl_funct,5,(fp),19)
	ASGNF_YC((fp),19,_dbl_global)
	ARGW_D(_int_global,(sp),0)
	ARGW_C(4,(sp),2)
	ARGW_C(Ltorture_test157,(sp),4)
	CALLV_C(_check,6)
	LEAVE
Ltorture_test159
	DW(1)
	DW(1)
	DW(2)
	DW(3)
	DW(5)
	DW(8)
Ltorture_test163
	STRING "hello"
	DB($0)

_simple_ptr_tests
	ENTER(1,26)
	ASGNS_CA(Ltorture_test159,(fp),6,12)
	ASGNW_AA((fp),6+2+2,(fp),18)
	ASGNS_CA(Ltorture_test163,(fp),20,6)
	ASGNW_AR((fp),20+2+1,reg0)
	ARGW_C(_tab_global,(sp),0)
	CALLW_CD(_int_ptr_funct,2,tmp0)
	MOVW_DY(tmp0,(fp),18)
	MOVW_YD((fp),18,tmp0)
	ARGW_D(tmp0,(sp),0)
	MOVW_CD(_tab_global+6,tmp0)
	ARGW_D(tmp0,(sp),2)
	ARGW_C(Ltorture_test168,(sp),4)
	CALLV_C(_check,6)
	ARGW_A((fp),6,(sp),0)
	CALLW_CD(_int_ptr_funct,2,tmp0)
	MOVW_DD(tmp0,_int_ptr_global)
	MOVW_DD(_int_ptr_global,tmp0)
	ARGW_D(tmp0,(sp),0)
	MOVW_AD((fp),6+6,tmp0)
	ARGW_D(tmp0,(sp),2)
	ARGW_C(Ltorture_test171,(sp),4)
	CALLV_C(_check,6)
	ADDW_DCD(reg0,2,tmp0)
	ARGW_D(tmp0,(sp),0)
	CALLW_CD(_char_ptr_funct,2,tmp0)
	MOVW_DD(tmp0,_char_ptr_global)
	MOVW_DD(_char_ptr_global,tmp0)
	ARGW_D(tmp0,(sp),0)
	ADDW_DCD(reg0,5,tmp0)
	ARGW_D(tmp0,(sp),2)
	ARGW_C(Ltorture_test173,(sp),4)
	CALLV_C(_check,6)
	ARGW_C(Ltorture_test175,(sp),0)
	CALLW_CD(_char_ptr_funct,2,tmp0)
	MOVW_DD(tmp0,reg0)
	CSBW_YD((reg0),0,tmp0)
	ARGW_D(tmp0,(sp),0)
	ARGW_C(108,(sp),2)
	ARGW_C(Ltorture_test177,(sp),4)
	CALLV_C(_check,6)
	LEAVE
_fct_ptr_tests
	ENTER(1,8)
	ASGNW_CA(_int_funct,(fp),6)
	ARGW_D(_int_global,(sp),0)
	CALLW_DD(_fct_ptr_global,2,reg0)
	ARGW_D(reg0,(sp),0)
	ADDW_DCD(_int_global,3,tmp0)
	ARGW_D(tmp0,(sp),2)
	ARGW_C(Ltorture_test179,(sp),4)
	CALLV_C(_check,6)
	ARGW_D(reg0,(sp),0)
	CALLW_YD((fp),6,2,_int_global)
	ARGW_D(_int_global,(sp),0)
	ADDW_DCD(reg0,3,tmp0)
	ARGW_D(tmp0,(sp),2)
	ARGW_C(Ltorture_test181,(sp),4)
	CALLV_C(_check,6)
	LEAVE
_struct_tests
	ENTER(1,10)
	ASGNW_AR((fp),6,reg0)
	ADDW_DCD(_struct_ptr_global,2,tmp0)
	ASGNW_CZ(1,tmp0)
	ARGW_D(_struct_global+2,(sp),0)
	ARGW_C(1,(sp),2)
	ARGW_C(Ltorture_test184,(sp),4)
	CALLV_C(_check,6)
	ARGS_C(_struct_global,(sp),0,4)
	MOVW_AD((fp),6,op1)
	CALLV_C(_struct_funct,4)
	ARGW_Y((fp),6+2,(sp),0)
	ARGW_C(4,(sp),2)
	ARGW_C(Ltorture_test188,(sp),4)
	CALLV_C(_check,6)
	ARGS_A((fp),6,(sp),0,4)
	MOVW_CD(_struct_global,op1)
	CALLV_C(_struct_funct,4)
	ARGW_D(_struct_global+2,(sp),0)
	ARGW_C(7,(sp),2)
	ARGW_C(Ltorture_test191,(sp),4)
	CALLV_C(_check,6)
	ARGW_D(_struct_ptr_global,(sp),0)
	CALLW_CD(_struct_ptr_funct,2,tmp0)
	MOVW_DD(tmp0,reg0)
	MOVW_DD(reg0,tmp0)
	ARGW_D(tmp0,(sp),0)
	MOVW_DD(_struct_ptr_global,tmp0)
	ARGW_D(tmp0,(sp),2)
	ARGW_C(Ltorture_test193,(sp),4)
	CALLV_C(_check,6)
	ADDW_DCD(reg0,2,tmp0)
	ARGW_Y((tmp0),0,(sp),0)
	ARGW_C(10,(sp),2)
	ARGW_C(Ltorture_test193,(sp),4)
	CALLV_C(_check,6)
	LEAVE
_main
	CALLV_C(_simple_tests,0)
	CALLV_C(_simple_ptr_tests,0)
	CALLV_C(_fct_ptr_tests,0)
	CALLV_C(_struct_tests,0)
	ARGW_C(Ltorture_test195,(sp),0)
	CALLV_C(_printf,2)
	RET
Ltorture_test195
	DB($a)

	STRING "All tests done"
	DB($a)
	DB($0)

Ltorture_test193
	STRING "struct ptr funct"
	DB($0)

Ltorture_test191
	STRING "struct funct2"
	DB($0)

Ltorture_test188
	STRING "struct funct1"
	DB($0)

Ltorture_test184
	STRING "struct funct0"
	DB($0)

Ltorture_test181
	STRING "fct ptr2"
	DB($0)

Ltorture_test179
	STRING "fct ptr1"
	DB($0)

Ltorture_test177
	STRING "char ptr funct2"
	DB($0)

Ltorture_test175
	STRING "world"
	DB($0)

Ltorture_test173
	STRING "char ptr funct1"
	DB($0)

Ltorture_test171
	STRING "int ptr funct2"
	DB($0)

Ltorture_test168
	STRING "int ptr funct1"
	DB($0)

Ltorture_test157
	STRING "dbl funct"
	DB($0)

Ltorture_test155
	STRING "flt funct"
	DB($0)

Ltorture_test153
	STRING "char funct"
	DB($0)

Ltorture_test151
	STRING "int funct2"
	DB($0)

Ltorture_test149
	STRING "int funct1"
	DB($0)

Ltorture_test147
	STRING "int funct0"
	DB($0)

Ltorture_test145
	DB($81)
	DB($0)
	DB($0)
	DB($0)
	DB($0)
Ltorture_test143
	STRING "%s test fails"
	DB($a)

	STRING "-> result is %d, expecting %d"
	DB($a)
	DB($0)

Ltorture_test141
	STRING "%s test ok"
	DB($a)
	DB($0)

Ltorture_test132
	DB($82)
	DB($40)
	DB($0)
	DB($0)
	DB($0)
Ltorture_test130
	DB($82)
	DB($40)
	DB($0)
	DB($0)
	DB($0)
