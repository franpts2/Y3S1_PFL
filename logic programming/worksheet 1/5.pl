job(technician, eleuterio).
job(technician, juvenaldo).
job(analyst, leonilde).
job(analyst, marciliano).
job(engineer, osvaldo).
job(engineer, porfirio).
job(engineer, reginaldo).
job(supervisor, sisnando).
job(chief_supervisor, gertrudes).
job(secretary, felismina).
job(director, asdrubal).
supervised_by(technician, engineer).
supervised_by(engineer, supervisor).
supervised_by(analyst, supervisor).
supervised_by(supervisor, chief_supervisor).
supervised_by(chief_supervisor, director).
supervised_by(secretary, director).

/* a) 
i. Does Sisnando have the job that supervises analysts?
ii. Which job supervises the one that supervises technician?
iii. What job and who is supervised by the supervisor?
iv. Which person does Asdubral supervise?

b)
i. yes
ii. supervisor
iii. leonilde
iv. gertrudes
*/

% c)
directSupervisor(X,Y):- job(JX,X), job(JY,Y), supervised_by(JX,JY).
supervisedBySameJob(X,Y):- job(JX,X), job(JY,Y), supervised_by(JX,Job), supervised_by(JY,Job).
supervisesMoreThanOneJob(X):- job(JX,X), supervised_by(J1,JX), supervised_by(J2,JX), J1\=J2.
supervisorOfSupervisor(X,Y):- job(JX,X), job(JY,Y), supervised_by(JX,J), supervised_by(J,JY).