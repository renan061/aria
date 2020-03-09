; ModuleID = 'aria.bc'
source_filename = "main.aria"

%struct_timespec = type { i64, i64 }
%Array = type { i8*, [5 x i8*]*, i32* }
%Array-P = type { i8*, [5 x i8*]*, i1 }

@zerostring = global i8 0
@size = global i32 undef
@runs = global i32 undef
@subruns = global i32 undef
@result = global i32 undef
@minFloat = global double undef
@maxFloat = global double undef
@Array-vmt-R = global [5 x i8*] undef
@Array-vmt-L = global [5 x i8*] undef
@Array-vmt-P = global [5 x i8*] undef
@msg = private unnamed_addr constant [38 x i8] c"runtime error: proxy function not ok\0A\00", align 1
@msg.1 = private unnamed_addr constant [38 x i8] c"runtime error: proxy function not ok\0A\00", align 1
@msg.2 = private unnamed_addr constant [38 x i8] c"runtime error: proxy function not ok\0A\00", align 1
@msg.3 = private unnamed_addr constant [38 x i8] c"runtime error: proxy function not ok\0A\00", align 1
@msg.4 = private unnamed_addr constant [38 x i8] c"runtime error: proxy function not ok\0A\00", align 1
@String = private unnamed_addr constant [2 x i8] c"\0A\00", align 1
@String.5 = private unnamed_addr constant [14 x i8] c"\09Avg: %f sec\0A\00", align 1
@String.6 = private unnamed_addr constant [25 x i8] c"\09Max: %f sec (+ %.1f%%)\0A\00", align 1
@String.7 = private unnamed_addr constant [25 x i8] c"\09Min: %f sec (- %.1f%%)\0A\00", align 1
@String.8 = private unnamed_addr constant [17 x i8] c"\09=> %.2f slower\0A\00", align 1
@String.9 = private unnamed_addr constant [2 x i8] c"\0A\00", align 1
@String.10 = private unnamed_addr constant [2 x i8] c"\0A\00", align 1
@String.11 = private unnamed_addr constant [12 x i8] c"sumR error\0A\00", align 1
@String.12 = private unnamed_addr constant [16 x i8] c"\09REGULAR ARRAY\0A\00", align 1
@String.13 = private unnamed_addr constant [13 x i8] c"sumMG error\0A\00", align 1
@String.14 = private unnamed_addr constant [26 x i8] c"\09ARRAY MONITOR USING GET\0A\00", align 1
@String.15 = private unnamed_addr constant [17 x i8] c"array.sum error\0A\00", align 1
@String.16 = private unnamed_addr constant [17 x i8] c"\09MONITOR METHOD\0A\00", align 1
@String.17 = private unnamed_addr constant [13 x i8] c"sumMG error\0A\00", align 1
@String.18 = private unnamed_addr constant [19 x i8] c"\09UNLOCKED MONITOR\0A\00", align 1

declare i32 @printf(i8*, ...)

declare i32 @rand()

declare void @srand(i32)

declare i32 @clock_gettime(i32, %struct_timespec*)

define void @assert(i1, i8*) {
entry:
  %t = icmp eq i1 true, %0
  br i1 %t, label %ok, label %error

error:                                            ; preds = %entry
  %2 = call i32 (i8*, ...) @printf(i8* %1)
  call void @exit(i32 1)
  unreachable

ok:                                               ; preds = %entry
  ret void
}

declare noalias i8* @malloc(i32)

declare void @exit(i32)

declare i32 @pthread_create(i8*, i8*, i8* (i8*)*, i8*)

declare void @pthread_exit(i8*)

declare i32 @pthread_mutex_init(i8*, i8*)

declare i32 @pthread_mutex_lock(i8*)

declare i32 @pthread_mutex_unlock(i8*)

declare i32 @pthread_cond_init(i8*, i8*)

declare i32 @pthread_cond_wait(i8*, i8*)

declare i32 @pthread_cond_signal(i8*)

declare i32 @pthread_cond_broadcast(i8*)

define void @Array-init() {
entry:
  store i8* bitcast (i8* (i8*)* @acquire-unlocked-R to i8*), i8** getelementptr inbounds ([5 x i8*], [5 x i8*]* @Array-vmt-R, i32 0, i32 0)
  store i8* bitcast (i8* (i8*)* @acquire-unlocked-R to i8*), i8** getelementptr inbounds ([5 x i8*], [5 x i8*]* @Array-vmt-L, i32 0, i32 0)
  store i8* bitcast (i8* (i8*)* @acquire-unlocked-P to i8*), i8** getelementptr inbounds ([5 x i8*], [5 x i8*]* @Array-vmt-P, i32 0, i32 0)
  store i8* bitcast (void (i8*)* @release-unlocked-R to i8*), i8** getelementptr inbounds ([5 x i8*], [5 x i8*]* @Array-vmt-R, i32 0, i32 1)
  store i8* bitcast (void (i8*)* @release-unlocked-R to i8*), i8** getelementptr inbounds ([5 x i8*], [5 x i8*]* @Array-vmt-L, i32 0, i32 1)
  store i8* bitcast (void (i8*)* @release-unlocked-P to i8*), i8** getelementptr inbounds ([5 x i8*], [5 x i8*]* @Array-vmt-P, i32 0, i32 1)
  store i8* bitcast (i32 (i8*, i32)* @get-R to i8*), i8** getelementptr inbounds ([5 x i8*], [5 x i8*]* @Array-vmt-R, i32 0, i32 2)
  store i8* bitcast (i32 (i8*, i32)* @get-L to i8*), i8** getelementptr inbounds ([5 x i8*], [5 x i8*]* @Array-vmt-L, i32 0, i32 2)
  store i8* bitcast (i32 (i8*, i32)* @get-P to i8*), i8** getelementptr inbounds ([5 x i8*], [5 x i8*]* @Array-vmt-P, i32 0, i32 2)
  store i8* bitcast (void (i8*, i32, i32)* @set-R to i8*), i8** getelementptr inbounds ([5 x i8*], [5 x i8*]* @Array-vmt-R, i32 0, i32 3)
  store i8* bitcast (void (i8*, i32, i32)* @set-L to i8*), i8** getelementptr inbounds ([5 x i8*], [5 x i8*]* @Array-vmt-L, i32 0, i32 3)
  store i8* bitcast (void (i8*, i32, i32)* @set-P to i8*), i8** getelementptr inbounds ([5 x i8*], [5 x i8*]* @Array-vmt-P, i32 0, i32 3)
  store i8* bitcast (i32 (i8*)* @sum-R to i8*), i8** getelementptr inbounds ([5 x i8*], [5 x i8*]* @Array-vmt-R, i32 0, i32 4)
  store i8* bitcast (i32 (i8*)* @sum-L to i8*), i8** getelementptr inbounds ([5 x i8*], [5 x i8*]* @Array-vmt-L, i32 0, i32 4)
  store i8* bitcast (i32 (i8*)* @sum-P to i8*), i8** getelementptr inbounds ([5 x i8*], [5 x i8*]* @Array-vmt-P, i32 0, i32 4)
  ret void
}

define i8* @Array(i32) {
entry:
  %malloccall = tail call i8* @malloc(i32 trunc (i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 3) to i32))
  %t = bitcast i8* %malloccall to %Array*
  %t1 = call i8* @malloc(i32 40)
  %t2 = call i32 @pthread_mutex_init(i8* %t1, i8* null)
  %t3 = getelementptr inbounds %Array, %Array* %t, i32 0, i32 0
  store i8* %t1, i8** %t3
  %vmt = getelementptr inbounds %Array, %Array* %t, i32 0, i32 1
  store [5 x i8*]* @Array-vmt-L, [5 x i8*]** %vmt
  %t4 = getelementptr inbounds %Array, %Array* %t, i32 0, i32 2
  store i32* undef, i32** %t4
  %t5 = getelementptr inbounds %Array, %Array* %t, i32 0, i32 2
  %i = alloca i32
  store i32 0, i32* %i
  %t6 = sub i32 %0, 0
  %mallocsize = mul i32 %t6, ptrtoint (i32* getelementptr (i32, i32* null, i32 1) to i32)
  %malloccall7 = tail call i8* @malloc(i32 %mallocsize)
  %t8 = bitcast i8* %malloccall7 to i32*
  %t9 = alloca i32
  store i32 0, i32* %t9
  br label %cond

loop:                                             ; preds = %cond
  %t14 = load i32, i32* %i
  %t15 = load i32, i32* %t9
  %t16 = getelementptr i32, i32* %t8, i32 %t15
  store i32 %t14, i32* %t16
  %t17 = add i32 %t15, 1
  store i32 %t17, i32* %t9
  br label %inc

end:                                              ; preds = %cond
  store i32* %t8, i32** %t5
  %t18 = bitcast %Array* %t to i8*
  ret i8* %t18

cond:                                             ; preds = %inc, %entry
  %t10 = load i32, i32* %i
  %t11 = icmp slt i32 %t10, %0
  br i1 %t11, label %loop, label %end

inc:                                              ; preds = %loop
  %t12 = load i32, i32* %i
  %t13 = add i32 %t12, 1
  store i32 %t13, i32* %i
  br label %cond
}

define i8* @acquire-unlocked-R(i8*) {
entry:
  %self = bitcast i8* %0 to %Array*
  ret i8* %0
}

define i8* @acquire-unlocked-P(i8*) {
entry:
  %proxy = bitcast i8* %0 to %Array-P*
  %ok1 = getelementptr inbounds %Array-P, %Array-P* %proxy, i32 0, i32 2
  %ok2 = load i1, i1* %ok1
  %t = icmp eq i1 %ok2, true
  br i1 %t, label %ok, label %error

error:                                            ; preds = %entry
  %1 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([38 x i8], [38 x i8]* @msg, i32 0, i32 0))
  call void @exit(i32 1)
  unreachable

ok:                                               ; preds = %entry
  %self = getelementptr inbounds %Array-P, %Array-P* %proxy, i32 0, i32 0
  %self3 = load i8*, i8** %self
  %ret = call i8* @acquire-unlocked-R(i8* %self3)
  ret i8* %ret
}

define void @release-unlocked-R(i8*) {
entry:
  %self = bitcast i8* %0 to %Array*
  ret void
}

define void @release-unlocked-P(i8*) {
entry:
  %proxy = bitcast i8* %0 to %Array-P*
  %ok1 = getelementptr inbounds %Array-P, %Array-P* %proxy, i32 0, i32 2
  %ok2 = load i1, i1* %ok1
  %t = icmp eq i1 %ok2, true
  br i1 %t, label %ok, label %error

error:                                            ; preds = %entry
  %1 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([38 x i8], [38 x i8]* @msg.1, i32 0, i32 0))
  call void @exit(i32 1)
  unreachable

ok:                                               ; preds = %entry
  %self = getelementptr inbounds %Array-P, %Array-P* %proxy, i32 0, i32 0
  %self3 = load i8*, i8** %self
  call void @release-unlocked-R(i8* %self3)
  ret void
}

define i32 @get-R(i8*, i32) {
entry:
  %self = bitcast i8* %0 to %Array*
  %t = getelementptr inbounds %Array, %Array* %self, i32 0, i32 2
  %t1 = load i32*, i32** %t
  %t2 = getelementptr i32, i32* %t1, i32 %1
  %t3 = load i32, i32* %t2
  ret i32 %t3
}

define i32 @get-L(i8*, i32) {
entry:
  %self = bitcast i8* %0 to %Array*
  %mutex-ptr = getelementptr inbounds %Array, %Array* %self, i32 0, i32 0
  %mutex = load i8*, i8** %mutex-ptr
  %t = call i32 @pthread_mutex_lock(i8* %mutex)
  %ret = call i32 @get-R(i8* %0, i32 %1)
  %t1 = call i32 @pthread_mutex_unlock(i8* %mutex)
  ret i32 %ret
}

define i32 @get-P(i8*, i32) {
entry:
  %proxy = bitcast i8* %0 to %Array-P*
  %ok1 = getelementptr inbounds %Array-P, %Array-P* %proxy, i32 0, i32 2
  %ok2 = load i1, i1* %ok1
  %t = icmp eq i1 %ok2, true
  br i1 %t, label %ok, label %error

error:                                            ; preds = %entry
  %2 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([38 x i8], [38 x i8]* @msg.2, i32 0, i32 0))
  call void @exit(i32 1)
  unreachable

ok:                                               ; preds = %entry
  %self = getelementptr inbounds %Array-P, %Array-P* %proxy, i32 0, i32 0
  %self3 = load i8*, i8** %self
  %ret = call i32 @get-R(i8* %self3, i32 %1)
  ret i32 %ret
}

define void @set-R(i8*, i32, i32) {
entry:
  %self = bitcast i8* %0 to %Array*
  %t = getelementptr inbounds %Array, %Array* %self, i32 0, i32 2
  %t1 = load i32*, i32** %t
  %t2 = getelementptr i32, i32* %t1, i32 %1
  store i32 %2, i32* %t2
  ret void
}

define void @set-L(i8*, i32, i32) {
entry:
  %self = bitcast i8* %0 to %Array*
  %mutex-ptr = getelementptr inbounds %Array, %Array* %self, i32 0, i32 0
  %mutex = load i8*, i8** %mutex-ptr
  %t = call i32 @pthread_mutex_lock(i8* %mutex)
  call void @set-R(i8* %0, i32 %1, i32 %2)
  %t1 = call i32 @pthread_mutex_unlock(i8* %mutex)
  ret void
}

define void @set-P(i8*, i32, i32) {
entry:
  %proxy = bitcast i8* %0 to %Array-P*
  %ok1 = getelementptr inbounds %Array-P, %Array-P* %proxy, i32 0, i32 2
  %ok2 = load i1, i1* %ok1
  %t = icmp eq i1 %ok2, true
  br i1 %t, label %ok, label %error

error:                                            ; preds = %entry
  %3 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([38 x i8], [38 x i8]* @msg.3, i32 0, i32 0))
  call void @exit(i32 1)
  unreachable

ok:                                               ; preds = %entry
  %self = getelementptr inbounds %Array-P, %Array-P* %proxy, i32 0, i32 0
  %self3 = load i8*, i8** %self
  call void @set-R(i8* %self3, i32 %1, i32 %2)
  ret void
}

define i32 @sum-R(i8*) {
entry:
  %self = bitcast i8* %0 to %Array*
  %x = alloca i32
  store i32 0, i32* %x
  %i = alloca i32
  store i32 0, i32* %i
  %t = load i32, i32* @size
  br label %cond

loop:                                             ; preds = %cond
  %t5 = load i32, i32* %x
  %t6 = getelementptr inbounds %Array, %Array* %self, i32 0, i32 2
  %t7 = load i32*, i32** %t6
  %t8 = load i32, i32* %i
  %t9 = getelementptr i32, i32* %t7, i32 %t8
  %t10 = load i32, i32* %t9
  %t11 = add i32 %t5, %t10
  store i32 %t11, i32* %x
  br label %inc

end:                                              ; preds = %cond
  %t12 = load i32, i32* %x
  ret i32 %t12

cond:                                             ; preds = %inc, %entry
  %t1 = load i32, i32* %i
  %t2 = icmp slt i32 %t1, %t
  br i1 %t2, label %loop, label %end

inc:                                              ; preds = %loop
  %t3 = load i32, i32* %i
  %t4 = add i32 %t3, 1
  store i32 %t4, i32* %i
  br label %cond
}

define i32 @sum-L(i8*) {
entry:
  %self = bitcast i8* %0 to %Array*
  %mutex-ptr = getelementptr inbounds %Array, %Array* %self, i32 0, i32 0
  %mutex = load i8*, i8** %mutex-ptr
  %t = call i32 @pthread_mutex_lock(i8* %mutex)
  %ret = call i32 @sum-R(i8* %0)
  %t1 = call i32 @pthread_mutex_unlock(i8* %mutex)
  ret i32 %ret
}

define i32 @sum-P(i8*) {
entry:
  %proxy = bitcast i8* %0 to %Array-P*
  %ok1 = getelementptr inbounds %Array-P, %Array-P* %proxy, i32 0, i32 2
  %ok2 = load i1, i1* %ok1
  %t = icmp eq i1 %ok2, true
  br i1 %t, label %ok, label %error

error:                                            ; preds = %entry
  %1 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([38 x i8], [38 x i8]* @msg.4, i32 0, i32 0))
  call void @exit(i32 1)
  unreachable

ok:                                               ; preds = %entry
  %self = getelementptr inbounds %Array-P, %Array-P* %proxy, i32 0, i32 0
  %self3 = load i8*, i8** %self
  %ret = call i32 @sum-R(i8* %self3)
  ret i32 %ret
}

define i32 @sumR(i32*) {
entry:
  %x = alloca i32
  store i32 0, i32* %x
  %i = alloca i32
  store i32 0, i32* %i
  %t = load i32, i32* @size
  br label %cond

loop:                                             ; preds = %cond
  %t5 = load i32, i32* %x
  %t6 = load i32, i32* %i
  %t7 = getelementptr i32, i32* %0, i32 %t6
  %t8 = load i32, i32* %t7
  %t9 = add i32 %t5, %t8
  store i32 %t9, i32* %x
  br label %inc

end:                                              ; preds = %cond
  %t10 = load i32, i32* %x
  ret i32 %t10

cond:                                             ; preds = %inc, %entry
  %t1 = load i32, i32* %i
  %t2 = icmp slt i32 %t1, %t
  br i1 %t2, label %loop, label %end

inc:                                              ; preds = %loop
  %t3 = load i32, i32* %i
  %t4 = add i32 %t3, 1
  store i32 %t4, i32* %i
  br label %cond
}

define i32 @sumMG(i8*) {
entry:
  %x = alloca i32
  store i32 0, i32* %x
  %i = alloca i32
  store i32 0, i32* %i
  %t = load i32, i32* @size
  br label %cond

loop:                                             ; preds = %cond
  %t5 = load i32, i32* %x
  %t6 = load i32, i32* %i
  %t7 = bitcast i8* %0 to %Array*
  %t8 = getelementptr inbounds %Array, %Array* %t7, i32 0, i32 1
  %vmt = load [5 x i8*]*, [5 x i8*]** %t8
  %t9 = getelementptr [5 x i8*], [5 x i8*]* %vmt, i32 0, i32 2
  %t10 = load i8*, i8** %t9
  %t11 = bitcast i8* %t10 to i32 (i8*, i32)*
  %1 = call i32 %t11(i8* %0, i32 %t6)
  %t12 = add i32 %t5, %1
  store i32 %t12, i32* %x
  br label %inc

end:                                              ; preds = %cond
  %t13 = load i32, i32* %x
  ret i32 %t13

cond:                                             ; preds = %inc, %entry
  %t1 = load i32, i32* %i
  %t2 = icmp slt i32 %t1, %t
  br i1 %t2, label %loop, label %end

inc:                                              ; preds = %loop
  %t3 = load i32, i32* %i
  %t4 = add i32 %t3, 1
  store i32 %t4, i32* %i
  br label %cond
}

define i32 @sumUM(i8*) {
entry:
  %x = alloca i32
  store i32 0, i32* %x

  %t = bitcast i8* %0 to %Array*
  ; %t1 = getelementptr inbounds %Array, %Array* %t, i32 0, i32 1
  ; %vmt = load [5 x i8*]*, [5 x i8*]** %t1
  ; %t2 = getelementptr [5 x i8*], [5 x i8*]* %vmt, i32 0, i32 0
  ; %t3 = load i8*, i8** %t2
  ; %t4 = bitcast i8* %t3 to i8* (i8*)*
  ; %1 = call i8* %t4(i8* %0)
  %malloccall = tail call i8* @malloc(i32 ptrtoint (%Array-P* getelementptr (%Array-P, %Array-P* null, i32 1) to i32))
  %proxy = bitcast i8* %malloccall to %Array-P*
  %obj = getelementptr inbounds %Array-P, %Array-P* %proxy, i32 0, i32 0
  ; store i8* %1, i8** %obj
  store i8* %0, i8** %obj
  %vmt5 = getelementptr inbounds %Array-P, %Array-P* %proxy, i32 0, i32 1
  store [5 x i8*]* @Array-vmt-R, [5 x i8*]** %vmt5
  ; %ok = getelementptr inbounds %Array-P, %Array-P* %proxy, i32 0, i32 2
  ; store i1 true, i1* %ok
  %proxy6 = bitcast %Array-P* %proxy to i8*
  ; %obj7 = bitcast i8* %1 to %Array*
  ; %mutex-ptr = getelementptr inbounds %Array, %Array* %obj7, i32 0, i32 0
  ; %mutex = load i8*, i8** %mutex-ptr
  ; %t8 = call i32 @pthread_mutex_lock(i8* %mutex)
  %i = alloca i32
  store i32 0, i32* %i
  %t9 = load i32, i32* @size
  br label %cond

loop:                                             ; preds = %cond
  %t14 = load i32, i32* %x
  %t15 = load i32, i32* %i
  %t16 = bitcast i8* %proxy6 to %Array*
  %t17 = getelementptr inbounds %Array, %Array* %t16, i32 0, i32 1
  %vmt18 = load [5 x i8*]*, [5 x i8*]** %t17
  %t19 = getelementptr [5 x i8*], [5 x i8*]* %vmt18, i32 0, i32 2
  %t20 = load i8*, i8** %t19
  %t21 = bitcast i8* %t20 to i32 (i8*, i32)*
  %1 = call i32 %t21(i8* %0, i32 %t15)
  %t22 = add i32 %t14, %1
  store i32 %t22, i32* %x
  br label %inc

end:                                              ; preds = %cond
  ; store i1 false, i1* %ok
  ; %t23 = call i32 @pthread_mutex_unlock(i8* %mutex)
  ; %proxy24 = bitcast i8* %0 to %Array*
  ; %t25 = getelementptr inbounds %Array, %Array* %proxy24, i32 0, i32 1
  ; %vmt26 = load [5 x i8*]*, [5 x i8*]** %t25
  ; %t27 = getelementptr [5 x i8*], [5 x i8*]* %vmt26, i32 0, i32 1
  ; %t28 = load i8*, i8** %t27
  ; %t29 = bitcast i8* %t28 to void (i8*)*
  ; call void %t29(i8* %0)
  %t30 = load i32, i32* %x
  ret i32 %t30

cond:                                             ; preds = %inc, %entry
  %t10 = load i32, i32* %i
  %t11 = icmp slt i32 %t10, %t9
  br i1 %t11, label %loop, label %end

inc:                                              ; preds = %loop
  %t12 = load i32, i32* %i
  %t13 = add i32 %t12, 1
  store i32 %t13, i32* %i
  br label %cond
}

define double @stats(double, double*) {
entry:
  %x = alloca double
  %avg = alloca double
  store double 0.000000e+00, double* %avg
  %min = alloca double
  %t = load double, double* @maxFloat
  store double %t, double* %min
  %max = alloca double
  %t1 = load double, double* @minFloat
  store double %t1, double* %max
  %i = alloca i32
  store i32 0, i32* %i
  %t2 = load i32, i32* @runs
  br label %cond

loop:                                             ; preds = %cond
  %t7 = load i32, i32* %i
  %t8 = getelementptr double, double* %1, i32 %t7
  %t9 = load double, double* %t8
  store double %t9, double* %x
  %t10 = load double, double* %avg
  %t11 = load double, double* %x
  %t12 = fadd double %t10, %t11
  store double %t12, double* %avg
  %t14 = load double, double* %x
  %t15 = load double, double* %min
  %t16 = fcmp olt double %t14, %t15
  br i1 %t16, label %if, label %end13

end:                                              ; preds = %cond
  %t24 = load double, double* %avg
  %t25 = load i32, i32* @runs
  %t26 = sitofp i32 %t25 to double
  %t27 = fdiv double %t24, %t26
  store double %t27, double* %avg
  %t28 = load double, double* %avg
  %t29 = fdiv double 1.000000e+02, %t28
  %t30 = load double, double* %max
  %t31 = load double, double* %avg
  %t32 = fsub double %t30, %t31
  %t33 = fmul double %t32, %t29
  %t34 = load double, double* %avg
  %t35 = load double, double* %min
  %t36 = fsub double %t34, %t35
  %t37 = fmul double %t36, %t29
  %2 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @String, i32 0, i32 0))
  %t38 = load double, double* %avg
  %3 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([14 x i8], [14 x i8]* @String.5, i32 0, i32 0), double %t38)
  %t39 = load double, double* %max
  %4 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([25 x i8], [25 x i8]* @String.6, i32 0, i32 0), double %t39, double %t33)
  %t40 = load double, double* %min
  %5 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([25 x i8], [25 x i8]* @String.7, i32 0, i32 0), double %t40, double %t37)
  %t43 = fcmp one double %0, 0.000000e+00
  br i1 %t43, label %if41, label %end42

cond:                                             ; preds = %inc, %entry
  %t3 = load i32, i32* %i
  %t4 = icmp slt i32 %t3, %t2
  br i1 %t4, label %loop, label %end

inc:                                              ; preds = %end19
  %t5 = load i32, i32* %i
  %t6 = add i32 %t5, 1
  store i32 %t6, i32* %i
  br label %cond

if:                                               ; preds = %loop
  %t17 = load double, double* %x
  store double %t17, double* %min
  br label %end13

end13:                                            ; preds = %if, %loop
  %t20 = load double, double* %x
  %t21 = load double, double* %max
  %t22 = fcmp ogt double %t20, %t21
  br i1 %t22, label %if18, label %end19

if18:                                             ; preds = %end13
  %t23 = load double, double* %x
  store double %t23, double* %max
  br label %end19

end19:                                            ; preds = %if18, %end13
  br label %inc

if41:                                             ; preds = %end
  %t44 = load double, double* %avg
  %t45 = fdiv double %t44, %0
  %6 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([17 x i8], [17 x i8]* @String.8, i32 0, i32 0), double %t45)
  br label %end42

end42:                                            ; preds = %if41, %end
  %7 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @String.9, i32 0, i32 0))
  %t46 = load double, double* %avg
  ret double %t46
}

define void @main() {
entry:
  store i32 100000000, i32* @size
  store i32 10, i32* @runs
  store i32 10, i32* @subruns
  store i32 887459712, i32* @result
  store double 0.000000e+00, double* @minFloat
  store double 1.000000e+10, double* @maxFloat
  call void @Array-init()
  %partials = alloca double*
  %t = load i32, i32* @runs
  %mallocsize = mul i32 %t, ptrtoint (double* getelementptr (double, double* null, i32 1) to i32)
  %malloccall = tail call i8* @malloc(i32 %mallocsize)
  %t1 = bitcast i8* %malloccall to double*
  store double* %t1, double** %partials
  %t2 = alloca double
  %base = alloca double
  %0 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @String.10, i32 0, i32 0))
  %i = alloca i32
  store i32 0, i32* %i
  %t3 = load i32, i32* @size
  %t4 = sub i32 %t3, 0
  %mallocsize5 = mul i32 %t4, ptrtoint (i32* getelementptr (i32, i32* null, i32 1) to i32)
  %malloccall6 = tail call i8* @malloc(i32 %mallocsize5)
  %t7 = bitcast i8* %malloccall6 to i32*
  %t8 = alloca i32
  store i32 0, i32* %t8
  br label %cond

loop:                                             ; preds = %cond
  %t13 = load i32, i32* %i
  %t14 = load i32, i32* %t8
  %t15 = getelementptr i32, i32* %t7, i32 %t14
  store i32 %t13, i32* %t15
  %t16 = add i32 %t14, 1
  store i32 %t16, i32* %t8
  br label %inc

end:                                              ; preds = %cond
  %i19 = alloca i32
  store i32 0, i32* %i19
  %t20 = load i32, i32* @runs
  br label %cond21

cond:                                             ; preds = %inc, %entry
  %t9 = load i32, i32* %i
  %t10 = icmp slt i32 %t9, %t3
  br i1 %t10, label %loop, label %end

inc:                                              ; preds = %loop
  %t11 = load i32, i32* %i
  %t12 = add i32 %t11, 1
  store i32 %t12, i32* %i
  br label %cond

loop17:                                           ; preds = %cond21
  %t27 = alloca %struct_timespec
  %1 = call i32 @clock_gettime(i32 0, %struct_timespec* %t27)
  %t28 = getelementptr inbounds %struct_timespec, %struct_timespec* %t27, i32 0, i32 0
  %t29 = load i64, i64* %t28
  %t30 = getelementptr inbounds %struct_timespec, %struct_timespec* %t27, i32 0, i32 1
  %t31 = load i64, i64* %t30
  %t32 = sitofp i64 %t31 to double
  %t33 = fdiv double %t32, 1.000000e+09
  %t34 = sitofp i64 %t29 to double
  %t35 = fadd double %t34, %t33
  store double %t35, double* %t2
  %j = alloca i32
  store i32 0, i32* %j
  %t38 = load i32, i32* @subruns
  br label %cond39

end18:                                            ; preds = %cond21
  %2 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([16 x i8], [16 x i8]* @String.12, i32 0, i32 0))
  %t62 = load double*, double** %partials
  %3 = call double @stats(double 0.000000e+00, double* %t62)
  store double %3, double* %base
  %t63 = load i32, i32* @size
  %4 = call i8* @Array(i32 %t63)
  %i66 = alloca i32
  store i32 0, i32* %i66
  %t67 = load i32, i32* @runs
  br label %cond68

cond21:                                           ; preds = %inc22, %end
  %t23 = load i32, i32* %i19
  %t24 = icmp slt i32 %t23, %t20
  br i1 %t24, label %loop17, label %end18

inc22:                                            ; preds = %end37
  %t25 = load i32, i32* %i19
  %t26 = add i32 %t25, 1
  store i32 %t26, i32* %i19
  br label %cond21

loop36:                                           ; preds = %cond39
  %5 = call i32 @sumR(i32* %t7)
  %t45 = load i32, i32* @result
  %t46 = icmp eq i32 %5, %t45
  br i1 %t46, label %a, label %b

end37:                                            ; preds = %cond39
  %t48 = load double*, double** %partials
  %t49 = load i32, i32* %i19
  %t50 = getelementptr double, double* %t48, i32 %t49
  %t51 = alloca %struct_timespec
  %6 = call i32 @clock_gettime(i32 0, %struct_timespec* %t51)
  %t52 = getelementptr inbounds %struct_timespec, %struct_timespec* %t51, i32 0, i32 0
  %t53 = load i64, i64* %t52
  %t54 = getelementptr inbounds %struct_timespec, %struct_timespec* %t51, i32 0, i32 1
  %t55 = load i64, i64* %t54
  %t56 = sitofp i64 %t55 to double
  %t57 = fdiv double %t56, 1.000000e+09
  %t58 = sitofp i64 %t53 to double
  %t59 = fadd double %t58, %t57
  %t60 = load double, double* %t2
  %t61 = fsub double %t59, %t60
  store double %t61, double* %t50
  br label %inc22

cond39:                                           ; preds = %inc40, %loop17
  %t41 = load i32, i32* %j
  %t42 = icmp slt i32 %t41, %t38
  br i1 %t42, label %loop36, label %end37

inc40:                                            ; preds = %phi
  %t43 = load i32, i32* %j
  %t44 = add i32 %t43, 1
  store i32 %t44, i32* %j
  br label %cond39

a:                                                ; preds = %loop36
  br label %phi

b:                                                ; preds = %loop36
  br label %phi

phi:                                              ; preds = %b, %a
  %phi47 = phi i1 [ true, %a ], [ false, %b ]
  call void @assert(i1 %phi47, i8* getelementptr inbounds ([12 x i8], [12 x i8]* @String.11, i32 0, i32 0))
  br label %inc40

loop64:                                           ; preds = %cond68
  %t74 = alloca %struct_timespec
  %7 = call i32 @clock_gettime(i32 0, %struct_timespec* %t74)
  %t75 = getelementptr inbounds %struct_timespec, %struct_timespec* %t74, i32 0, i32 0
  %t76 = load i64, i64* %t75
  %t77 = getelementptr inbounds %struct_timespec, %struct_timespec* %t74, i32 0, i32 1
  %t78 = load i64, i64* %t77
  %t79 = sitofp i64 %t78 to double
  %t80 = fdiv double %t79, 1.000000e+09
  %t81 = sitofp i64 %t76 to double
  %t82 = fadd double %t81, %t80
  store double %t82, double* %t2
  %j85 = alloca i32
  store i32 0, i32* %j85
  %t86 = load i32, i32* @subruns
  br label %cond87

end65:                                            ; preds = %cond68
  %8 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([26 x i8], [26 x i8]* @String.14, i32 0, i32 0))
  %t113 = load double, double* %base
  %t114 = load double*, double** %partials
  %9 = call double @stats(double %t113, double* %t114)
  %t115 = load i32, i32* @size
  %10 = call i8* @Array(i32 %t115)
  %i118 = alloca i32
  store i32 0, i32* %i118
  %t119 = load i32, i32* @runs
  br label %cond120

cond68:                                           ; preds = %inc69, %end18
  %t70 = load i32, i32* %i66
  %t71 = icmp slt i32 %t70, %t67
  br i1 %t71, label %loop64, label %end65

inc69:                                            ; preds = %end84
  %t72 = load i32, i32* %i66
  %t73 = add i32 %t72, 1
  store i32 %t73, i32* %i66
  br label %cond68

loop83:                                           ; preds = %cond87
  %11 = call i32 @sumMG(i8* %4)
  %t96 = load i32, i32* @result
  %t97 = icmp eq i32 %11, %t96
  br i1 %t97, label %a93, label %b94

end84:                                            ; preds = %cond87
  %t99 = load double*, double** %partials
  %t100 = load i32, i32* %i66
  %t101 = getelementptr double, double* %t99, i32 %t100
  %t102 = alloca %struct_timespec
  %12 = call i32 @clock_gettime(i32 0, %struct_timespec* %t102)
  %t103 = getelementptr inbounds %struct_timespec, %struct_timespec* %t102, i32 0, i32 0
  %t104 = load i64, i64* %t103
  %t105 = getelementptr inbounds %struct_timespec, %struct_timespec* %t102, i32 0, i32 1
  %t106 = load i64, i64* %t105
  %t107 = sitofp i64 %t106 to double
  %t108 = fdiv double %t107, 1.000000e+09
  %t109 = sitofp i64 %t104 to double
  %t110 = fadd double %t109, %t108
  %t111 = load double, double* %t2
  %t112 = fsub double %t110, %t111
  store double %t112, double* %t101
  br label %inc69

cond87:                                           ; preds = %inc88, %loop64
  %t89 = load i32, i32* %j85
  %t90 = icmp slt i32 %t89, %t86
  br i1 %t90, label %loop83, label %end84

inc88:                                            ; preds = %phi95
  %t91 = load i32, i32* %j85
  %t92 = add i32 %t91, 1
  store i32 %t92, i32* %j85
  br label %cond87

a93:                                              ; preds = %loop83
  br label %phi95

b94:                                              ; preds = %loop83
  br label %phi95

phi95:                                            ; preds = %b94, %a93
  %phi98 = phi i1 [ true, %a93 ], [ false, %b94 ]
  call void @assert(i1 %phi98, i8* getelementptr inbounds ([13 x i8], [13 x i8]* @String.13, i32 0, i32 0))
  br label %inc88

loop116:                                          ; preds = %cond120
  %t126 = alloca %struct_timespec
  %13 = call i32 @clock_gettime(i32 0, %struct_timespec* %t126)
  %t127 = getelementptr inbounds %struct_timespec, %struct_timespec* %t126, i32 0, i32 0
  %t128 = load i64, i64* %t127
  %t129 = getelementptr inbounds %struct_timespec, %struct_timespec* %t126, i32 0, i32 1
  %t130 = load i64, i64* %t129
  %t131 = sitofp i64 %t130 to double
  %t132 = fdiv double %t131, 1.000000e+09
  %t133 = sitofp i64 %t128 to double
  %t134 = fadd double %t133, %t132
  store double %t134, double* %t2
  %j137 = alloca i32
  store i32 0, i32* %j137
  %t138 = load i32, i32* @subruns
  br label %cond139

end117:                                           ; preds = %cond120
  %14 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([17 x i8], [17 x i8]* @String.16, i32 0, i32 0))
  %t170 = load double, double* %base
  %t171 = load double*, double** %partials
  %15 = call double @stats(double %t170, double* %t171)
  %t172 = load i32, i32* @size
  %16 = call i8* @Array(i32 %t172)
  %i175 = alloca i32
  store i32 0, i32* %i175
  %t176 = load i32, i32* @runs
  br label %cond177

cond120:                                          ; preds = %inc121, %end65
  %t122 = load i32, i32* %i118
  %t123 = icmp slt i32 %t122, %t119
  br i1 %t123, label %loop116, label %end117

inc121:                                           ; preds = %end136
  %t124 = load i32, i32* %i118
  %t125 = add i32 %t124, 1
  store i32 %t125, i32* %i118
  br label %cond120

loop135:                                          ; preds = %cond139
  %t148 = bitcast i8* %10 to %Array*
  %t149 = getelementptr inbounds %Array, %Array* %t148, i32 0, i32 1
  %vmt = load [5 x i8*]*, [5 x i8*]** %t149
  %t150 = getelementptr [5 x i8*], [5 x i8*]* %vmt, i32 0, i32 4
  %t151 = load i8*, i8** %t150
  %t152 = bitcast i8* %t151 to i32 (i8*)*
  %17 = call i32 %t152(i8* %10)
  %t153 = load i32, i32* @result
  %t154 = icmp eq i32 %17, %t153
  br i1 %t154, label %a145, label %b146

end136:                                           ; preds = %cond139
  %t156 = load double*, double** %partials
  %t157 = load i32, i32* %i118
  %t158 = getelementptr double, double* %t156, i32 %t157
  %t159 = alloca %struct_timespec
  %18 = call i32 @clock_gettime(i32 0, %struct_timespec* %t159)
  %t160 = getelementptr inbounds %struct_timespec, %struct_timespec* %t159, i32 0, i32 0
  %t161 = load i64, i64* %t160
  %t162 = getelementptr inbounds %struct_timespec, %struct_timespec* %t159, i32 0, i32 1
  %t163 = load i64, i64* %t162
  %t164 = sitofp i64 %t163 to double
  %t165 = fdiv double %t164, 1.000000e+09
  %t166 = sitofp i64 %t161 to double
  %t167 = fadd double %t166, %t165
  %t168 = load double, double* %t2
  %t169 = fsub double %t167, %t168
  store double %t169, double* %t158
  br label %inc121

cond139:                                          ; preds = %inc140, %loop116
  %t141 = load i32, i32* %j137
  %t142 = icmp slt i32 %t141, %t138
  br i1 %t142, label %loop135, label %end136

inc140:                                           ; preds = %phi147
  %t143 = load i32, i32* %j137
  %t144 = add i32 %t143, 1
  store i32 %t144, i32* %j137
  br label %cond139

a145:                                             ; preds = %loop135
  br label %phi147

b146:                                             ; preds = %loop135
  br label %phi147

phi147:                                           ; preds = %b146, %a145
  %phi155 = phi i1 [ true, %a145 ], [ false, %b146 ]
  call void @assert(i1 %phi155, i8* getelementptr inbounds ([17 x i8], [17 x i8]* @String.15, i32 0, i32 0))
  br label %inc140

loop173:                                          ; preds = %cond177
  %t183 = alloca %struct_timespec
  %19 = call i32 @clock_gettime(i32 0, %struct_timespec* %t183)
  %t184 = getelementptr inbounds %struct_timespec, %struct_timespec* %t183, i32 0, i32 0
  %t185 = load i64, i64* %t184
  %t186 = getelementptr inbounds %struct_timespec, %struct_timespec* %t183, i32 0, i32 1
  %t187 = load i64, i64* %t186
  %t188 = sitofp i64 %t187 to double
  %t189 = fdiv double %t188, 1.000000e+09
  %t190 = sitofp i64 %t185 to double
  %t191 = fadd double %t190, %t189
  store double %t191, double* %t2
  %j194 = alloca i32
  store i32 0, i32* %j194
  %t195 = load i32, i32* @subruns
  br label %cond196

end174:                                           ; preds = %cond177
  %20 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([19 x i8], [19 x i8]* @String.18, i32 0, i32 0))
  %t222 = load double, double* %base
  %t223 = load double*, double** %partials
  %21 = call double @stats(double %t222, double* %t223)
  call void @pthread_exit(i8* null)
  ret void

cond177:                                          ; preds = %inc178, %end117
  %t179 = load i32, i32* %i175
  %t180 = icmp slt i32 %t179, %t176
  br i1 %t180, label %loop173, label %end174

inc178:                                           ; preds = %end193
  %t181 = load i32, i32* %i175
  %t182 = add i32 %t181, 1
  store i32 %t182, i32* %i175
  br label %cond177

loop192:                                          ; preds = %cond196
  %22 = call i32 @sumUM(i8* %16)
  %t205 = load i32, i32* @result
  %t206 = icmp eq i32 %22, %t205
  br i1 %t206, label %a202, label %b203

end193:                                           ; preds = %cond196
  %t208 = load double*, double** %partials
  %t209 = load i32, i32* %i175
  %t210 = getelementptr double, double* %t208, i32 %t209
  %t211 = alloca %struct_timespec
  %23 = call i32 @clock_gettime(i32 0, %struct_timespec* %t211)
  %t212 = getelementptr inbounds %struct_timespec, %struct_timespec* %t211, i32 0, i32 0
  %t213 = load i64, i64* %t212
  %t214 = getelementptr inbounds %struct_timespec, %struct_timespec* %t211, i32 0, i32 1
  %t215 = load i64, i64* %t214
  %t216 = sitofp i64 %t215 to double
  %t217 = fdiv double %t216, 1.000000e+09
  %t218 = sitofp i64 %t213 to double
  %t219 = fadd double %t218, %t217
  %t220 = load double, double* %t2
  %t221 = fsub double %t219, %t220
  store double %t221, double* %t210
  br label %inc178

cond196:                                          ; preds = %inc197, %loop173
  %t198 = load i32, i32* %j194
  %t199 = icmp slt i32 %t198, %t195
  br i1 %t199, label %loop192, label %end193

inc197:                                           ; preds = %phi204
  %t200 = load i32, i32* %j194
  %t201 = add i32 %t200, 1
  store i32 %t201, i32* %j194
  br label %cond196

a202:                                             ; preds = %loop192
  br label %phi204

b203:                                             ; preds = %loop192
  br label %phi204

phi204:                                           ; preds = %b203, %a202
  %phi207 = phi i1 [ true, %a202 ], [ false, %b203 ]
  call void @assert(i1 %phi207, i8* getelementptr inbounds ([13 x i8], [13 x i8]* @String.17, i32 0, i32 0))
  br label %inc197
}
