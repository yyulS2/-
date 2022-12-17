# install.packages("rJava")
# install.packages("DBI")
# install.packages("RJDBC")

Sys.setenv(JAVA_HOME="C://Program Files//Java//jdk1.8.0_202")

library(DBI)
library(rJava)
library(RJDBC)




drv <- JDBC("oracle.jdbc.driver.OracleDriver","C:/OracleTest/ojdbc6.jar")
conn <- dbConnect(drv,"jdbc:oracle:thin:@//127.0.0.1:1521/xe","scott","tiger")



query="SELECT*FROM test_table"
dbGetQuery(conn,query)
# ID PASS   NAME AGE
# 1 hong 1234 홍길동  35
# 2  kim 5678 김길동  45

query="SELECT*FROM test_table order by age desc"
dbGetQuery(conn,query)
# ID PASS   NAME AGE
# 1  kim 5678 김길동  45
# 2 hong 1234 홍길동  35

query="insert into test_table values('kang','1234','강감찬',45)"
dbSendUpdate(conn, query)



query = "SELECT*FROM test_table"
dbGetQuery(conn,query)
#    ID PASS   NAME AGE
# 1 hong 1234 홍길동  35
# 2  kim 5678 김길동  45
# 3 kang 1234 강감찬  45

query="select*from test_table where age >=40"
result <- dbGetQuery(conn,query)
result
# ID PASS   NAME AGE
# 1  kim 5678 김길동  45
# 2 kang 1234 강감찬  45

query = "update test_table set age = 40 where name = '강감찬'"
dbSendUpdate(conn,query)



query = "select*from test_table where name ='강감찬'"
dbGetQuery(conn,query)
# ID PASS   NAME AGE
# 1 kang 1234 강감찬  40



query="delete from test_table where name = '홍길동'"
dbSendUpdate(conn,query)


query="select*from test_table"
dbGetQuery(conn,query)
# ID PASS   NAME AGE
# 1  kim 5678 김길동  45
# 2 kang 1234 강감찬  40




# PDF _ SQL2

query = "select avg(sal)from emp group by deptno"
dbGetQuery(conn,query)

query = "select max(sal) from emp group by deptno having max(sal) > 500"
dbGetQuery(conn,query)

query = "create table myboard (
num number(4) primary key,
author varchar2(12),
title varchar2(30),
content varchar2(60)
)"
dbSendUpdate(conn,query)
query="select*from myboard"
dbGetQuery(conn,query)

query="create sequence myboard_seq"
dbSendUpdate(conn,query)

query = "insert into myboard(num, author, title, content) values(myboard_seq.nextval, '전우치', '제목','내용이다.')"
dbSendUpdate(conn,query)

query="select*from myboard"
dbGetQuery(conn,query)



query = "select * from emp, dept"
dbGetQuery(conn, query)

query = "select empno,ename,sal,dept.deptno from emp, dept where emp.deptno = dept.deptno"
dbGetQuery(conn,query)

query = "select e.empno, e.ename, e.sal, d.deptno,d.dname from emp e, dept d 
where e.deptno = d.deptno and d.deptno = 30"
dbGetQuery(conn,query)


query = " select e.empno,e.ename,e.sal,d.deptno,d.dname from emp e, dept d 
where e.deptno = d.deptno and e.comm is not null"
dbGetQuery(conn,query)




query = ""





