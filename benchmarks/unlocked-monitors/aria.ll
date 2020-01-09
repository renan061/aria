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

define i32 @get-R(i8*, i32) alwaysinline {
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
  br label %cond

cond:                                             ; preds = %loop, %entry
  %t = load i32, i32* %i
  %t1 = load i32, i32* @size
  %t2 = icmp slt i32 %t, %t1
  br i1 %t2, label %loop, label %end

loop:                                             ; preds = %cond
  %t3 = load i32, i32* %x
  %t4 = getelementptr inbounds %Array, %Array* %self, i32 0, i32 2
  %t5 = load i32*, i32** %t4
  %t6 = load i32, i32* %i
  %t7 = getelementptr i32, i32* %t5, i32 %t6
  %t8 = load i32, i32* %t7
  %t9 = add i32 %t3, %t8
  store i32 %t9, i32* %x
  %t10 = load i32, i32* %i
  %t11 = add i32 %t10, 1
  store i32 %t11, i32* %i
  br label %cond

end:                                              ; preds = %cond
  %t12 = load i32, i32* %x
  ret i32 %t12
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
  br label %cond

cond:                                             ; preds = %loop, %entry
  %t = load i32, i32* %i
  %t1 = load i32, i32* @size
  %t2 = icmp slt i32 %t, %t1
  br i1 %t2, label %loop, label %end

loop:                                             ; preds = %cond
  %t3 = load i32, i32* %x
  %t4 = load i32, i32* %i
  %t5 = getelementptr i32, i32* %0, i32 %t4
  %t6 = load i32, i32* %t5
  %t7 = add i32 %t3, %t6
  store i32 %t7, i32* %x
  %t8 = load i32, i32* %i
  %t9 = add i32 %t8, 1
  store i32 %t9, i32* %i
  br label %cond

end:                                              ; preds = %cond
  %t10 = load i32, i32* %x
  ret i32 %t10
}

define i32 @sumMG(i8*) {
entry:
  %x = alloca i32
  store i32 0, i32* %x
  %i = alloca i32
  store i32 0, i32* %i
  br label %cond

cond:                                             ; preds = %loop, %entry
  %t = load i32, i32* %i
  %t1 = load i32, i32* @size
  %t2 = icmp slt i32 %t, %t1
  br i1 %t2, label %loop, label %end

loop:                                             ; preds = %cond
  %t3 = load i32, i32* %x
  %t4 = load i32, i32* %i
  %t5 = bitcast i8* %0 to %Array*
  %t6 = getelementptr inbounds %Array, %Array* %t5, i32 0, i32 1
  %vmt = load [5 x i8*]*, [5 x i8*]** %t6
  %t7 = getelementptr [5 x i8*], [5 x i8*]* %vmt, i32 0, i32 2
  %t8 = load i8*, i8** %t7
  %t9 = bitcast i8* %t8 to i32 (i8*, i32)*
  %1 = call i32 %t9(i8* %0, i32 %t4)
  %t10 = add i32 %t3, %1
  store i32 %t10, i32* %x
  %t11 = load i32, i32* %i
  %t12 = add i32 %t11, 1
  store i32 %t12, i32* %i
  br label %cond

end:                                              ; preds = %cond
  %t13 = load i32, i32* %x
  ret i32 %t13
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
  ; %malloccall = tail call i8* @malloc(i32 ptrtoint (%Array-P* getelementptr (%Array-P, %Array-P* null, i32 1) to i32))
  ; %proxy = bitcast i8* %malloccall to %Array-P*
  ; %obj = getelementptr inbounds %Array-P, %Array-P* %proxy, i32 0, i32 0
  ; store i8* %1, i8** %obj
  ; %vmt5 = getelementptr inbounds %Array-P, %Array-P* %proxy, i32 0, i32 1
  ; store [5 x i8*]* @Array-vmt-P, [5 x i8*]** %vmt5
  ; %ok = getelementptr inbounds %Array-P, %Array-P* %proxy, i32 0, i32 2
  ; store i1 true, i1* %ok
  ; %proxy6 = bitcast %Array-P* %proxy to i8*
  ; %obj7 = bitcast i8* %1 to %Array*
  ; %mutex-ptr = getelementptr inbounds %Array, %Array* %obj7, i32 0, i32 0
  ; %mutex = load i8*, i8** %mutex-ptr
  ; %t8 = call i32 @pthread_mutex_lock(i8* %mutex)

  %i = alloca i32
  store i32 0, i32* %i
  br label %cond

cond:                                             ; preds = %loop, %entry
  %t9 = load i32, i32* %i
  %t10 = load i32, i32* @size
  %t11 = icmp slt i32 %t9, %t10
  br i1 %t11, label %loop, label %end

loop:                                             ; preds = %cond
  %t12 = load i32, i32* %x
  %t13 = load i32, i32* %i
  ; %t14 = bitcast i8* %proxy6 to %Array*
  ; %t15 = getelementptr inbounds %Array, %Array* %t, i32 0, i32 1
  ; %vmt16 = load [5 x i8*]*, [5 x i8*]** %t15
  ; %t17 = getelementptr [5 x i8*], [5 x i8*]* %vmt16, i32 0, i32 2
  ; %t18 = load i8*, i8** %t17
  ; %t19 = bitcast i8* %t18 to i32 (i8*, i32)*
  %1 = call i32 @get-R(i8* %0, i32 %t13)
  %t20 = add i32 %t12, %1
  store i32 %t20, i32* %x
  %t21 = load i32, i32* %i
  %t22 = add i32 %t21, 1
  store i32 %t22, i32* %i
  br label %cond

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
  br label %cond

cond:                                             ; preds = %end17, %entry
  %t2 = load i32, i32* %i
  %t3 = load i32, i32* @runs
  %t4 = icmp slt i32 %t2, %t3
  br i1 %t4, label %loop, label %end

loop:                                             ; preds = %cond
  %t5 = load i32, i32* %i
  %t6 = getelementptr double, double* %1, i32 %t5
  %t7 = load double, double* %t6
  store double %t7, double* %x
  %t8 = load double, double* %avg
  %t9 = load double, double* %x
  %t10 = fadd double %t8, %t9
  store double %t10, double* %avg
  %t12 = load double, double* %x
  %t13 = load double, double* %min
  %t14 = fcmp olt double %t12, %t13
  br i1 %t14, label %if, label %end11

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

if:                                               ; preds = %loop
  %t15 = load double, double* %x
  store double %t15, double* %min
  br label %end11

end11:                                            ; preds = %if, %loop
  %t18 = load double, double* %x
  %t19 = load double, double* %max
  %t20 = fcmp ogt double %t18, %t19
  br i1 %t20, label %if16, label %end17

if16:                                             ; preds = %end11
  %t21 = load double, double* %x
  store double %t21, double* %max
  br label %end17

end17:                                            ; preds = %if16, %end11
  %t22 = load i32, i32* %i
  %t23 = add i32 %t22, 1
  store i32 %t23, i32* %i
  br label %cond

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
  %i20 = alloca i32
  store i32 0, i32* %i20
  br label %cond17

cond:                                             ; preds = %inc, %entry
  %t9 = load i32, i32* %i
  %t10 = icmp slt i32 %t9, %t3
  br i1 %t10, label %loop, label %end

inc:                                              ; preds = %loop
  %t11 = load i32, i32* %i
  %t12 = add i32 %t11, 1
  store i32 %t12, i32* %i
  br label %cond

cond17:                                           ; preds = %end35, %end
  %t21 = load i32, i32* %i20
  %t22 = load i32, i32* @runs
  %t23 = icmp slt i32 %t21, %t22
  br i1 %t23, label %loop18, label %end19

loop18:                                           ; preds = %cond17
  %t24 = alloca %struct_timespec
  %1 = call i32 @clock_gettime(i32 0, %struct_timespec* %t24)
  %t25 = getelementptr inbounds %struct_timespec, %struct_timespec* %t24, i32 0, i32 0
  %t26 = load i64, i64* %t25
  %t27 = getelementptr inbounds %struct_timespec, %struct_timespec* %t24, i32 0, i32 1
  %t28 = load i64, i64* %t27
  %t29 = sitofp i64 %t28 to double
  %t30 = fdiv double %t29, 1.000000e+09
  %t31 = sitofp i64 %t26 to double
  %t32 = fadd double %t31, %t30
  store double %t32, double* %t2
  %j = alloca i32
  store i32 0, i32* %j
  br label %cond33

end19:                                            ; preds = %cond17
  %2 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([16 x i8], [16 x i8]* @String.12, i32 0, i32 0))
  %t60 = load double*, double** %partials
  %3 = call double @stats(double 0.000000e+00, double* %t60)
  store double %3, double* %base
  %t61 = load i32, i32* @size
  %4 = call i8* @Array(i32 %t61)
  %i65 = alloca i32
  store i32 0, i32* %i65
  br label %cond62

cond33:                                           ; preds = %phi, %loop18
  %t36 = load i32, i32* %j
  %t37 = load i32, i32* @subruns
  %t38 = icmp slt i32 %t36, %t37
  br i1 %t38, label %loop34, label %end35

loop34:                                           ; preds = %cond33
  %5 = call i32 @sumR(i32* %t7)
  %t39 = load i32, i32* @result
  %t40 = icmp eq i32 %5, %t39
  br i1 %t40, label %a, label %b

end35:                                            ; preds = %cond33
  %t44 = load double*, double** %partials
  %t45 = load i32, i32* %i20
  %t46 = getelementptr double, double* %t44, i32 %t45
  %t47 = alloca %struct_timespec
  %6 = call i32 @clock_gettime(i32 0, %struct_timespec* %t47)
  %t48 = getelementptr inbounds %struct_timespec, %struct_timespec* %t47, i32 0, i32 0
  %t49 = load i64, i64* %t48
  %t50 = getelementptr inbounds %struct_timespec, %struct_timespec* %t47, i32 0, i32 1
  %t51 = load i64, i64* %t50
  %t52 = sitofp i64 %t51 to double
  %t53 = fdiv double %t52, 1.000000e+09
  %t54 = sitofp i64 %t49 to double
  %t55 = fadd double %t54, %t53
  %t56 = load double, double* %t2
  %t57 = fsub double %t55, %t56
  store double %t57, double* %t46
  %t58 = load i32, i32* %i20
  %t59 = add i32 %t58, 1
  store i32 %t59, i32* %i20
  br label %cond17

a:                                                ; preds = %loop34
  br label %phi

b:                                                ; preds = %loop34
  br label %phi

phi:                                              ; preds = %b, %a
  %phi41 = phi i1 [ true, %a ], [ false, %b ]
  call void @assert(i1 %phi41, i8* getelementptr inbounds ([12 x i8], [12 x i8]* @String.11, i32 0, i32 0))
  %t42 = load i32, i32* %j
  %t43 = add i32 %t42, 1
  store i32 %t43, i32* %j
  br label %cond33

cond62:                                           ; preds = %end80, %end19
  %t66 = load i32, i32* %i65
  %t67 = load i32, i32* @runs
  %t68 = icmp slt i32 %t66, %t67
  br i1 %t68, label %loop63, label %end64

loop63:                                           ; preds = %cond62
  %t69 = alloca %struct_timespec
  %7 = call i32 @clock_gettime(i32 0, %struct_timespec* %t69)
  %t70 = getelementptr inbounds %struct_timespec, %struct_timespec* %t69, i32 0, i32 0
  %t71 = load i64, i64* %t70
  %t72 = getelementptr inbounds %struct_timespec, %struct_timespec* %t69, i32 0, i32 1
  %t73 = load i64, i64* %t72
  %t74 = sitofp i64 %t73 to double
  %t75 = fdiv double %t74, 1.000000e+09
  %t76 = sitofp i64 %t71 to double
  %t77 = fadd double %t76, %t75
  store double %t77, double* %t2
  %j81 = alloca i32
  store i32 0, i32* %j81
  br label %cond78

end64:                                            ; preds = %cond62
  %8 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([26 x i8], [26 x i8]* @String.14, i32 0, i32 0))
  %t109 = load double, double* %base
  %t110 = load double*, double** %partials
  %9 = call double @stats(double %t109, double* %t110)
  %t111 = load i32, i32* @size
  %10 = call i8* @Array(i32 %t111)
  %i115 = alloca i32
  store i32 0, i32* %i115
  br label %cond112

cond78:                                           ; preds = %phi87, %loop63
  %t82 = load i32, i32* %j81
  %t83 = load i32, i32* @subruns
  %t84 = icmp slt i32 %t82, %t83
  br i1 %t84, label %loop79, label %end80

loop79:                                           ; preds = %cond78
  %11 = call i32 @sumMG(i8* %4)
  %t88 = load i32, i32* @result
  %t89 = icmp eq i32 %11, %t88
  br i1 %t89, label %a85, label %b86

end80:                                            ; preds = %cond78
  %t93 = load double*, double** %partials
  %t94 = load i32, i32* %i65
  %t95 = getelementptr double, double* %t93, i32 %t94
  %t96 = alloca %struct_timespec
  %12 = call i32 @clock_gettime(i32 0, %struct_timespec* %t96)
  %t97 = getelementptr inbounds %struct_timespec, %struct_timespec* %t96, i32 0, i32 0
  %t98 = load i64, i64* %t97
  %t99 = getelementptr inbounds %struct_timespec, %struct_timespec* %t96, i32 0, i32 1
  %t100 = load i64, i64* %t99
  %t101 = sitofp i64 %t100 to double
  %t102 = fdiv double %t101, 1.000000e+09
  %t103 = sitofp i64 %t98 to double
  %t104 = fadd double %t103, %t102
  %t105 = load double, double* %t2
  %t106 = fsub double %t104, %t105
  store double %t106, double* %t95
  %t107 = load i32, i32* %i65
  %t108 = add i32 %t107, 1
  store i32 %t108, i32* %i65
  br label %cond62

a85:                                              ; preds = %loop79
  br label %phi87

b86:                                              ; preds = %loop79
  br label %phi87

phi87:                                            ; preds = %b86, %a85
  %phi90 = phi i1 [ true, %a85 ], [ false, %b86 ]
  call void @assert(i1 %phi90, i8* getelementptr inbounds ([13 x i8], [13 x i8]* @String.13, i32 0, i32 0))
  %t91 = load i32, i32* %j81
  %t92 = add i32 %t91, 1
  store i32 %t92, i32* %j81
  br label %cond78

cond112:                                          ; preds = %end130, %end64
  %t116 = load i32, i32* %i115
  %t117 = load i32, i32* @runs
  %t118 = icmp slt i32 %t116, %t117
  br i1 %t118, label %loop113, label %end114

loop113:                                          ; preds = %cond112
  %t119 = alloca %struct_timespec
  %13 = call i32 @clock_gettime(i32 0, %struct_timespec* %t119)
  %t120 = getelementptr inbounds %struct_timespec, %struct_timespec* %t119, i32 0, i32 0
  %t121 = load i64, i64* %t120
  %t122 = getelementptr inbounds %struct_timespec, %struct_timespec* %t119, i32 0, i32 1
  %t123 = load i64, i64* %t122
  %t124 = sitofp i64 %t123 to double
  %t125 = fdiv double %t124, 1.000000e+09
  %t126 = sitofp i64 %t121 to double
  %t127 = fadd double %t126, %t125
  store double %t127, double* %t2
  %j131 = alloca i32
  store i32 0, i32* %j131
  br label %cond128

end114:                                           ; preds = %cond112
  %14 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([17 x i8], [17 x i8]* @String.16, i32 0, i32 0))
  %t164 = load double, double* %base
  %t165 = load double*, double** %partials
  %15 = call double @stats(double %t164, double* %t165)
  %t166 = load i32, i32* @size
  %16 = call i8* @Array(i32 %t166)
  %i170 = alloca i32
  store i32 0, i32* %i170
  br label %cond167

cond128:                                          ; preds = %phi137, %loop113
  %t132 = load i32, i32* %j131
  %t133 = load i32, i32* @subruns
  %t134 = icmp slt i32 %t132, %t133
  br i1 %t134, label %loop129, label %end130

loop129:                                          ; preds = %cond128
  %t138 = bitcast i8* %10 to %Array*
  %t139 = getelementptr inbounds %Array, %Array* %t138, i32 0, i32 1
  %vmt = load [5 x i8*]*, [5 x i8*]** %t139
  %t140 = getelementptr [5 x i8*], [5 x i8*]* %vmt, i32 0, i32 4
  %t141 = load i8*, i8** %t140
  %t142 = bitcast i8* %t141 to i32 (i8*)*
  %17 = call i32 %t142(i8* %10)
  %t143 = load i32, i32* @result
  %t144 = icmp eq i32 %17, %t143
  br i1 %t144, label %a135, label %b136

end130:                                           ; preds = %cond128
  %t148 = load double*, double** %partials
  %t149 = load i32, i32* %i115
  %t150 = getelementptr double, double* %t148, i32 %t149
  %t151 = alloca %struct_timespec
  %18 = call i32 @clock_gettime(i32 0, %struct_timespec* %t151)
  %t152 = getelementptr inbounds %struct_timespec, %struct_timespec* %t151, i32 0, i32 0
  %t153 = load i64, i64* %t152
  %t154 = getelementptr inbounds %struct_timespec, %struct_timespec* %t151, i32 0, i32 1
  %t155 = load i64, i64* %t154
  %t156 = sitofp i64 %t155 to double
  %t157 = fdiv double %t156, 1.000000e+09
  %t158 = sitofp i64 %t153 to double
  %t159 = fadd double %t158, %t157
  %t160 = load double, double* %t2
  %t161 = fsub double %t159, %t160
  store double %t161, double* %t150
  %t162 = load i32, i32* %i115
  %t163 = add i32 %t162, 1
  store i32 %t163, i32* %i115
  br label %cond112

a135:                                             ; preds = %loop129
  br label %phi137

b136:                                             ; preds = %loop129
  br label %phi137

phi137:                                           ; preds = %b136, %a135
  %phi145 = phi i1 [ true, %a135 ], [ false, %b136 ]
  call void @assert(i1 %phi145, i8* getelementptr inbounds ([17 x i8], [17 x i8]* @String.15, i32 0, i32 0))
  %t146 = load i32, i32* %j131
  %t147 = add i32 %t146, 1
  store i32 %t147, i32* %j131
  br label %cond128

cond167:                                          ; preds = %end185, %end114
  %t171 = load i32, i32* %i170
  %t172 = load i32, i32* @runs
  %t173 = icmp slt i32 %t171, %t172
  br i1 %t173, label %loop168, label %end169

loop168:                                          ; preds = %cond167
  %t174 = alloca %struct_timespec
  %19 = call i32 @clock_gettime(i32 0, %struct_timespec* %t174)
  %t175 = getelementptr inbounds %struct_timespec, %struct_timespec* %t174, i32 0, i32 0
  %t176 = load i64, i64* %t175
  %t177 = getelementptr inbounds %struct_timespec, %struct_timespec* %t174, i32 0, i32 1
  %t178 = load i64, i64* %t177
  %t179 = sitofp i64 %t178 to double
  %t180 = fdiv double %t179, 1.000000e+09
  %t181 = sitofp i64 %t176 to double
  %t182 = fadd double %t181, %t180
  store double %t182, double* %t2
  %j186 = alloca i32
  store i32 0, i32* %j186
  br label %cond183

end169:                                           ; preds = %cond167
  %20 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([19 x i8], [19 x i8]* @String.18, i32 0, i32 0))
  %t214 = load double, double* %base
  %t215 = load double*, double** %partials
  %21 = call double @stats(double %t214, double* %t215)
  call void @pthread_exit(i8* null)
  ret void

cond183:                                          ; preds = %phi192, %loop168
  %t187 = load i32, i32* %j186
  %t188 = load i32, i32* @subruns
  %t189 = icmp slt i32 %t187, %t188
  br i1 %t189, label %loop184, label %end185

loop184:                                          ; preds = %cond183
  %22 = call i32 @sumUM(i8* %16)
  %t193 = load i32, i32* @result
  %t194 = icmp eq i32 %22, %t193
  br i1 %t194, label %a190, label %b191

end185:                                           ; preds = %cond183
  %t198 = load double*, double** %partials
  %t199 = load i32, i32* %i170
  %t200 = getelementptr double, double* %t198, i32 %t199
  %t201 = alloca %struct_timespec
  %23 = call i32 @clock_gettime(i32 0, %struct_timespec* %t201)
  %t202 = getelementptr inbounds %struct_timespec, %struct_timespec* %t201, i32 0, i32 0
  %t203 = load i64, i64* %t202
  %t204 = getelementptr inbounds %struct_timespec, %struct_timespec* %t201, i32 0, i32 1
  %t205 = load i64, i64* %t204
  %t206 = sitofp i64 %t205 to double
  %t207 = fdiv double %t206, 1.000000e+09
  %t208 = sitofp i64 %t203 to double
  %t209 = fadd double %t208, %t207
  %t210 = load double, double* %t2
  %t211 = fsub double %t209, %t210
  store double %t211, double* %t200
  %t212 = load i32, i32* %i170
  %t213 = add i32 %t212, 1
  store i32 %t213, i32* %i170
  br label %cond167

a190:                                             ; preds = %loop184
  br label %phi192

b191:                                             ; preds = %loop184
  br label %phi192

phi192:                                           ; preds = %b191, %a190
  %phi195 = phi i1 [ true, %a190 ], [ false, %b191 ]
  call void @assert(i1 %phi195, i8* getelementptr inbounds ([13 x i8], [13 x i8]* @String.17, i32 0, i32 0))
  %t196 = load i32, i32* %j186
  %t197 = add i32 %t196, 1
  store i32 %t197, i32* %j186
  br label %cond183
}
