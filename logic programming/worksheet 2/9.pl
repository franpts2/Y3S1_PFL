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


superior(X,Y):- 
	job(PX,X),
	job(PY,Y),
	supervised_by(PY,PX).

superior(X,Y):- 
	job(PX,X),
	job(PY,Y),
	job(PZ,Z),
	supervised_by(PY,PZ),
	superior(X,Z).	