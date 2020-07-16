(require 'cl-dbi)
(require 'cl-base64)
(require 'ironclad)
(require 'cl-smtp)
(require 'local-time)
(require 'hunchentoot)

(defun write-config (username password database
		     &optional (port nil) (server nil))
  (with-open-file (out "diab.config" :direction :output :if-exists :supersede)
    (format out "~a~%" ";this is an automatically generatet file")
    (format out "~a~%" ";do only edit it if you know what you are doing")
    (print
     (list :username username :password password
	   :database database :port port :server server)
     out)))

(defun read-config ()
  (with-open-file (in "diab.config" :direction :input)
    (read in)))

(defun write-contact-data (name email address)
  (with-open-file (out "contact.config" :direction :output
		       :if-exists :supersede)
    (format out "~a~%" ";this is an automatically generated file")
    (format out "~a~%" ";do only edit it if you know what you are doing")
    (print (list :name name :mail email :address address))))

(defun read-contact-data ()
  (with-open-file (in "contact.config" :direction :input)
    (read in)))

(defun get-connection ( settings )
  (if (getf settings :port)
      (if (getf settings :server)
	  (dbi:connect :mysql :database-name (getf settings :database)
		       :username (getf settings :username)
		       :password (getf settings :password)
		       :host (getf settings :server)
		       :port (getf settings :port))
	  (dbi:connect :mysql :database-name (getf settings :database)
		       :username (getf settings :username)
		       :password (getf settings :password)
		       :port (getf settings :port)))
      (if (getf settings :server)
	  (dbi:connect :mysql :database-name (getf settings :database)
		       :username (getf settings :username)
		       :password (getf settings :password)
		       :host (getf settings :server))
	  (dbi:connect :mysql :database-name (getf settings :database)
		       :username (getf settings :username)
		       :password (getf settings :password)))))

(defun encode (input-string)
  (cl-base64:string-to-base64-string input-string))

(defun decode (encoded-string)
  (if encoded-string
      (cl-base64:base64-string-to-string encoded-string)
      ""))

(defun install-basic-tables ()
  (let ((connection (get-connection (read-config))))
    (dbi:do-sql connection "create table if not exists users ( id int primary key auto_increment, name text, password text, bz text, kh text, email text )" ())
    (dbi:do-sql connection "create table if not exists accesslevels (username text, tablename text, level text )" ())
    (dbi:do-sql connection "create table if not exists pwrenew (username text, secret text, stamp bigint)" ())
    (dbi:disconnect connection)))

(defun add-new-user (username email password)
  (let ((connection (get-connection (read-config))))
    (ironclad:pbkdf2-hash-password
		       (ironclad:ascii-string-to-byte-array password))
    (dbi:do-sql connection "insert into users (name, password, email) values ( ?, ?, ?)"
		(list (encode username)
		      (encode
		       (ironclad:pbkdf2-hash-password-to-combined-string
			(ironclad:ascii-string-to-byte-array password)))
		      (encode email)))
    (dbi:disconnect connection)))

(defun valid-units-p (bz kh)
  (and (or (string-equal bz "mg") (string-equal bz "mmol"))
       (or (string-equal kh "be") (string-equal kh "khe")
	   (string-equal kh "g"))))

(defun add-med (userid medname medunit)
  (let (medid (connection (get-connection (read-config))))
    (dbi:do-sql connection
      "insert into medi? (name, unit) values ( ?, ? )"
      (list userid medname medunit))
    (setf medid (getf
		 (dbi:fetch
		  (dbi:execute
		   (dbi:prepare connection
			"select id from medi? where name = ? and unit = ?")
		   (list userid medname medunit))) :|id|))
    (dbi:do-sql connection
      "alter table sugar_values? add column med? float"
      (list userid medid))
    (dbi:disconnect connection)))

(defun create-user-tables (username bz kh medi)
  (if (valid-units-p bz kh)
      (let ((connection (get-connection (read-config))) userid)
	(setf userid
	      (getf
	       (dbi:fetch
		(dbi:execute
		 (dbi:prepare connection
			      "select id from users where name = ?")
		 (list (encode username)))) :|id|))
	(dbi:do-sql connection
	  "create table if not exists medi? (id int primary key auto_increment, name text, unit text)"
	      (list userid))
	(dbi:do-sql connection
	  "create table if not exists sugar_values? (id int primary key auto_increment, zeit datetime, value float, food float, remark text)"
	  (list userid))
	(dbi:do-sql connection
	  "insert into accesslevels (username, tablename, level) values (?, \"sugar_values?\", ?)"
	  (list (encode username) userid "o"))
	(dbi:do-sql connection
	  "update users set bz=?, kh=? where id=?"
	  (list bz kh userid))
	(do () ((not medi))
	  (add-med userid (first medi) (first (rest medi)))
	  (setf medi (rest (rest medi))))
	(dbi:disconnect connection))
      "impossible, wrong units"))

(defun login-valid-p (username password)
  (let ((connection (get-connection (read-config))) pw-from-db query)
    (setf query (dbi:execute
		(dbi:prepare connection
			     "select password from users where name = ?")
		(list (encode username))))
    (setf pw-from-db (dbi:fetch query))
    (dbi:disconnect connection)
    (if pw-from-db
	(ironclad:pbkdf2-check-password
	 (ironclad:ascii-string-to-byte-array password)
	 (decode (getf pw-from-db :|password|)))
	())))
    
(defun delete-user (username)
  (let (userid (connection (get-connection (read-config))))
    (setf userid (getf
		  (dbi:fetch
		   (dbi:execute
		    (dbi:prepare connection
				 "select id from users where name = ?" )
		    (list (encode username))))
		  :|id| ))
    (dbi:do-sql connection "drop table if exists sugar_values?" (list userid))
    (dbi:do-sql connection "drop table if exists medi?" (list userid))
    (dbi:do-sql connection "delete from accesslevels where username = ?"
		(list (encode username)))
    (dbi:do-sql connection "delete from users where id = ?" (list userid))
    (dbi:disconnect connection)))

(defun write-mailconfig (smtp-server own-email username password
			 &optional (port 25) (encryption nil))
  (with-open-file (out "diab-mail.config"
		       :direction :output :if-exists :supersede)
    (format out "~a~%" ";this is an automatically generatet file")
    (format out "~a~%" ";do only edit it if you know what you are doing")
    (print
     (list :mailserver smtp-server :port port :address own-email
	   :username username :password password :crypto encryption)
     out)))

(defun get-mailconfig ()
  (with-open-file (in "diab-mail.config" :direction :input)
    (read in)))

(defun send-message (recipient subject message)
  (let ((config (get-mailconfig)))
    (if (getf config :crypto)
	(cl-smtp:send-email (getf config :mailserver) (getf config :address)
			    recipient subject message :display-name
			    "Diabetiker Onlinetagebuch"
			    :authentication (list (getf config :username)
						  (getf config :password))
			    :port (getf config :port)
			    :ssl (getf config :crypto))
	(cl-smtp:send-email (getf config :mailserver) (getf config :address)
			    recipient subject message :display-name
			    "Diabetiker Onlinetagebuch"
			    :authentication (list (getf config :username)
						  (getf config :password))
			    :port (getf config :port)))))

(defun request-password-reset (username)
  (let ((secret (+ (* (length username) (get-universal-time)) (random 1000000)))
	(connection (get-connection(read-config))))
    (dbi:do-sql connection
      "insert into pwrenew (username, secret, stamp) values (?, ?, ?)"
      (list (encode username) secret (get-universal-time)))
    (dbi:disconnect connection)
    secret))

(defun reset-password (secret new-password)
  (let (result (connection (get-connection (read-config))))
    (setf result
	  (dbi:fetch
	   (dbi:execute
	    (dbi:prepare connection
			 "select * from pwrenew where secret = ?")
	    (list secret))))
    (if result
	(if (>= (* 60 60 5) (- (get-universal-time) (getf result :|stamp|)))
	    (progn
	      (dbi:do-sql
		connection
	      "update users set password = ? where name = ?"
	      (list (encode
		     (ironclad:pbkdf2-hash-password-to-combined-string
		      (ironclad:ascii-string-to-byte-array new-password)))
		    (getf result :|username|)))
	      (dbi:do-sql connection "delete from pwrenew where secret = ?"
		(list secret)))
	    "too late")
	"no valid request")
    (dbi:disconnect connection)))
    
(defun change-password (username new-password)
  (let ((connection (get-connection (read-config))))
    (dbi:do-sql connection "update users set password = ? where name = ?"
		(list (encode (ironclad:pbkdf2-hash-password-to-combined-string
			       (ironclad:ascii-string-to-byte-array
				new-password)))
		      (encode username)))
    (dbi:disconnect connection)))

(defun delete-entry (userid dataset-id)
  (let ((connection (get-connection (read-config))))
    (dbi:do-sql connection "delete from sugar_values? where id = ?"
      (list userid dataset-id))
    (dbi:disconnect connection)))

(defun insert-entry (userid timestamp value food remark &rest medication)
  (let ((connection (get-connection (read-config)))
	(parameters (list userid))
	(sql-string "insert into sugar_values? (zeit, value, food, remark"))
    (dotimes (i (length medication))
      (setf sql-string (concatenate 'string sql-string ", med?" ))
      (setf parameters (append parameters (list (+ 1 i)))))
    (setf sql-string (concatenate 'string sql-string ") values( ?, ?, ?, ?"))
    (dotimes (i (length medication))
      (setf sql-string (concatenate 'string sql-string ", ?")))
    (setf sql-string (concatenate 'string sql-string ")"))
    (setf parameters
	  (append parameters
		  (list timestamp value food (encode remark))
		  medication))
    (dbi:do-sql connection sql-string parameters)
    (dbi:disconnect connection)))

(defun change-entry
    (userid entryid timestamp value food remark &rest medication)
  (let ((connection (get-connection (read-config)))
	(query-string
	 "update sugar_values? set zeit=?, value=?, food=?, remark=?")
	(parameters (list userid timestamp value food remark))
	(counter 1))
    (dolist (current medication)
      (setf query-string (concatenate 'string query-string ", med?=?"))
      (setf parameters (append parameters (list counter current)))
      (setf counter (+ counter 1)))
    (setf query-string (concatenate 'string query-string " where id = ?"))
    (setf parameters (append parameters (list entryid)))
    (dbi:do-sql connection query-string parameters)
    (dbi:disconnect connection)))

(defun get-userid ( username )
  (let ((connection (get-connection (read-config))) userid)
    (setf userid
	  (getf (dbi:fetch
		 (dbi:execute
		  (dbi:prepare connection
			       "select * from users where name = ?")
		  (list (encode username))))
		:|id|))
    (dbi:disconnect connection)
    userid))

(defun get-username (userid)
  (let ((connection (get-connection (read-config))) username)
    (setf username
	  (decode
	   (getf
	    (dbi:fetch
	     (dbi:execute
	      (dbi:prepare connection
			   "select name from users where id = ?")
	      (list userid)))
	    :|name|)))
    username))

(defun get-accessible-lists (username)
  (let ((table-list ()) (level-list ())
	(connection (get-connection (read-config))) result current)
    (setf result (dbi:execute
		  (dbi:prepare connection
		   "select * from accesslevels where username = ?")
		  (list (encode username))))
    (setf current (dbi:fetch result))
    (do () ((not current))
      (setf table-list (append table-list (list (getf current :|tablename|))))
      (setf level-list (append level-list (list (getf current :|level|))))
      (setf current (dbi:fetch result)))
    (dbi:disconnect connection)
    (list table-list level-list)))

(defun get-date-string (date)
  (local-time:format-timestring nil (local-time:universal-to-timestamp date)
				:format '(:year "-" :month "-" :day " "
					  :hour ":" :min ":" :sec)))

(defun get-now-string ()
  (local-time:format-timestring nil (local-time:now)
				:format '(:year "-" :month "-" :day " "
					  :hour ":" :min ":" :sec)))

(defun get-query-results (sql-string parameters)
  (let ((return-value ()) (connection (get-connection (read-config))))
    (setf return-value (dbi:fetch-all
			(dbi:execute
			 (dbi:prepare connection sql-string)
			 parameters)))
    (dbi:disconnect connection)
    return-value))

(defun has-table-p (userid)
  (if (get-query-results "show tables like'sugar_values?'" (list userid))
      T
      ()))

(defun week-statistics (userid max-days-offset min-days-offset)
  (if (and (numberp max-days-offset) (numberp min-days-offset))
  (get-query-results
   "select weekday(zeit), hour(zeit), avg(value) from sugar_values? where date_sub(curdate(), interval ? day) <= zeit and date_sub(curdate(), interval ? day) > zeit and value is not null group by weekday(zeit), hour(zeit) asc"
   (list userid max-days-offset (- min-days-offset 1)))
  ()))
    
(defun day-statistics (userid max-days-offset min-days-offset)
  (if (and (numberp max-days-offset) (numberp min-days-offset))
  (get-query-results
   "select hour(zeit), avg(value) from sugar_values? where date_sub(curdate(), interval ? day) <= zeit and date_sub(curdate(), interval ? day) > zeit and value is not null group by hour(zeit)"
   (list userid max-days-offset (- min-days-offset 1)))
  ()))
    
(defun values-from-interval (userid max-days-offset min-days-offset)
  (get-query-results
   "select * from sugar_values? where date_sub(curdate(), interval ? day) <= zeit and date_sub(curdate(), interval ? day) > zeit order by zeit asc"
   (list userid max-days-offset (- min-days-offset 1))))

(defun get-statistics (userid max-days-offset min-days-offset)
  (if (and (numberp max-days-offset) (numberp min-days-offset))
  (get-query-results
   "select avg(value), max(value), min(value), stddev_pop(value), count(value) from sugar_values? where date_sub(curdate(), interval ? day) <= zeit and date_sub(curdate(), interval ? day) > zeit"
   (list userid max-days-offset (- min-days-offset 1)))
  ()))

(defun get-last-values (userid number-of-values)
  (if (numberp number-of-values)
  (get-query-results
   "(select * from sugar_values? order by zeit desc limit ?) order by zeit asc"
   (list userid number-of-values))
  ()))

(defun get-max-privlevel (username target-userid)
  (let ((sql-string "select * from accesslevels where username = ? and tablename = 'sugar_values?' and level = ?"))
    (if (get-query-results
	 sql-string (list (encode username) target-userid "o"))
	"o"
	(if (get-query-results
	     sql-string (list (encode username) target-userid "w"))
	    "rw"
	    (if (get-query-results
		 sql-string (list (encode username) target-userid "r"))
		"r"
		nil)))))

(defun has-write-priv-p (username target-userid)
  (let ((privlevel (get-max-privlevel username target-userid)))
    (or (string-equal privlevel "o") (string-equal privlevel "rw"))))

(defun has-read-priv-p (username target-userid)
  (if (get-max-privlevel username target-userid)
      T
      nil))

(defun has-owner-priv-p (username target-userid)
  (string-equal (get-max-privlevel username target-userid) "o"))

(defun user-exists-p (username)
  (if (get-query-results "select * from users where name=?"
			 (list (encode username)))
      t
      ()))

(defun make-html-site (content)
  (setf (hunchentoot:content-type*) "text/html")
  (concatenate 'string
	       "<!doctype html>
<head>
<title>Blutzuckermesswerte Webservice</title>
<body>"
	       content
	       "<br>
<br>
<img src=\"https://moredhel.is-a-geek.net/lisp.jpg\" alt=\"made with Lisp\">
</img>
</body>
</html>"))

(defun do-installation ()
  )

(defun get-login-html ()
   "Willkommen. Um diesen Dienst zu nutzen, muss man sich anmelden.<br>
<br>
<form action=\"?op=login\" method=post>
<table><tr><td>Username:</td><td><input type=text name=\"username\"></td></tr>
<tr><td>Passwort:</td><td><input type=password name=\"password\"></td></tr>
</table>
<input type=submit value=\"login\"></form>
<br><br><br>
<a href=\"?op=newuser\">neuen Nutzer anlegen</a>
<br><br>
<a href=\"\?op=resetpw\">ich habe mein Passwort vergessen</a>
<br><br><br>")

(defun get-string (data)
  (if data
      (if (numberp data)
	  (format nil "~,1f" data)
	  (format nil "~a" data))
      ""))

(defun show-login-form ()
  (make-html-site (get-login-html)))

(defun make-main-table (userid values-list)
  (let ((medi-list (get-query-results "select * from medi?" (list userid)))
	(unitlist
	 (get-query-results "select bz, kh from users where id = ?"
			    (list userid)))
	(return-string "<table><tr><td>Zeit</td><td>Messwert/"))
    (cond
      ((string-equal (getf (first unitlist) :|bz|) "mg")
       (setf return-string (concatenate 'string return-string "mg/dl")))
      ((string-equal (getf (first unitlist) :|bz|) "mmol")
       (setf return-string (concatenate 'string return-string "mMol/L"))))
    (setf return-string (concatenate 'string return-string
				     "</td><td>Mahlzeit/"))
    (cond
      ((string-equal (getf (first unitlist) :|kh|) "be")
       (setf return-string (concatenate 'string return-string "BE")))
      ((string-equal (getf (first unitlist) :|kh|) "khe")
       (setf return-string (concatenate 'string return-string "KHE")))
      ((string-equal (getf (first unitlist) :|kh|) "g")
       (setf return-string (concatenate 'string return-string "g"))))
    (setf return-string (concatenate 'string return-string
				     "</td><td>Bemerkung</td>"))
    (dolist (current-med medi-list)
	     (setf return-string
		   (concatenate 'string return-string "<td>"
				(getf current-med :|name|) "/"
				(getf current-med :|unit|)
				"</td>")))
    (setf return-string
	  (concatenate 'string return-string "<td></td><td></td></tr>"))
    (dolist (current values-list)
      (setf return-string
	    (concatenate 'string return-string
			 "<form action=?op=change method=post><tr><td>"
			 "<input type=\"hidden\" name=entry value="
			 (format () "~a" (getf current :|id|))
			 "><input type=text name=time value=\""
			 (get-date-string (getf current :|zeit|))
			 "\"></td><td><input type=text name=sugar value="
			 (get-string (getf current :|value|))
			 "></td><td><input type=text name=food value="
			 (get-string (getf current :|food|))
			 "></td><td><input type=text name=remark value=\""
			 (decode (getf current :|remark|))
			 "\"></td>"))
        (dotimes (i (length medi-list))
	(setf return-string (concatenate 'string return-string "<td>"
					 "<input type=text name=med"
					 (format () "~a" i)
					 " value="
					 (get-string (nth (+ 11 (* 2 i))
							  current)) "></td>")))
      (setf return-string
	    (concatenate 'string
			 return-string
			 "<td><input type=submit value=\"Werte &auml;ndern\">"
			 "</form></td>"
			 "<td><form action=?op=delete method=post>"
			 "<input type=hidden name=id value="
			 (format () "~a" (getf current :|id|))
			 "><input type=submit value=\"Datensatz l&ouml;schen\">"
			 "</form></td></tr>")))
    (setf return-string (concatenate 'string
				     return-string
				     "<form action=?op=addnew method=post><tr>"
				     "<td><input type=text name=time value=\""
				     (get-now-string)
				     "\"></td><td>"
				     "<input type=text name=sugar></td>"
				     "<td><input type=text name=food></td>"
				     "<td><input type=text name=remark></td>"))
    (dotimes (i (length medi-list))
      (setf return-string (concatenate 'string return-string
				       "<td><input type=text name=med"
				       (format () "~a" i)
				       "></td>")))
    (concatenate 'string return-string
		 "<td><input type=submit value=\"neuen Datensatz speichern\">"
		 "</form>"
		 "</td><td></td></tr></table><br><br>")))

(defun get-menu-html ()
  (let ((return-string ""))
    (if (has-table-p (hunchentoot:session-value 'userid))
	(setf return-string (concatenate 'string return-string
		  "<form action=?op=showlast method=post>"
		  "letzte <input type=number name=value-count value=30>"
		  "Werte <input type=submit value=anzeigen></form><br>"
		  "<form action=?op=value-interval method=post>"
		  "Werte anzeigen von vor"
		  "<input type=number name=start value=30>"
		  "bis vor"
		  "<input type=number name=stop value=0>"
		  "Tagen<input type=submit value=anzeigen>"
		  "</form><br>"
		  "<form action=?op=daystat method=post>"
		  "Durchschnittliches Tagesprofil anzeigen von vor"
		  "<input type=number name=start value=30>"
		  "bis vor"
		  "<input type=number name=stop value=0>"
		  "Tagen<input type=submit value=anzeigen>"
		  "</form><br>"
		  "<form action=?op=weekstat method=post>"
		  "Durchschnittliches Wochenprofil anzeigen von vor"
		  "<input type=number name=start value=30>"
		  "bis vor"
		  "<input type=number name=stop value=0>"
		  "Tagen<input type=submit value=anzeigen>"
		  "</form><br>"
		  "<form action=?op=stat method=post>"
		  "Statistik anzeigen von vor"
		  "<input type=number name=start value=90>"
		  "bis vor"
		  "<input type=number name=stop value=0>"
		  "Tagen<input type=submit value=anzeigen>"
		  "</form><br><br>")))
    (let ((accessibles (get-accessible-lists
			(hunchentoot:session-value 'username))))
      (if (> (length (first accessibles)) 1)
	  (dotimes (i (length (first accessibles)))
	    (setf return-string
		  (concatenate 'string return-string
			       "<a href=?op=chuser&uid="
			       (subseq (nth i (first accessibles)) 12)
			       ">Tabelle von Nutzer "
			       (get-username (subseq
					      (nth i (first accessibles)) 12))
			       " verwenden</a><br>")))))
    (setf return-string
	  (concatenate 'string return-string
		       (if (has-table-p (hunchentoot:session-value 'userid))
			   (concatenate 'string
					"<br><br><a href=\"?op=givepriv\">"
					"anderen Nutzern Rechte geben oder"
					" widerrufen</a><br>"))
		 "<a href=\"?op=changepw\">Passwort &auml;ndern</a><br>"
		 "<a href=\"?op=deleteuser\">Benutzerkonto l&ouml;schen</a><br>"
		 "<a href=?op=kontakt>Kontaktdaten des Betreibers anzeigen</a>"
		 "<br><a href=?op=terms>Nutzungsbedingungen anzeigen</a>"
		 "<br><br>"))
    return-string))

(defun main-content ()
  (let ((html-string "")
	(value-count (hunchentoot:parameter "value-count")))
    (if (not value-count) (setf value-count "30"))
    (if (string-equal value-count "") (setf value-count "30"))
    (if (has-table-p (hunchentoot:session-value 'userid))
    (setf html-string
	  (concatenate 'string html-string
		       "<h3>Die letzen "
		       value-count
		       " Messwerte sind:</h3><br>"
		       (make-main-table
			(hunchentoot:session-value 'userid)
			(get-last-values
			 (hunchentoot:session-value 'userid)
			 (parse-integer value-count :junk-allowed t)))
		       (get-menu-html)))
    (setf html-string (concatenate 'string
				    "keine Tabelle f&uuml;r Messwerte "
				    "vorhanden."
				    (if (= (hunchentoot:session-value 'userid)
					   (hunchentoot:session-value
					    'own-userid))
					(concatenate 'string
						     "<a href=?op=maketable>"
						     "Hier</a> klicken um "
						     "eine zu erstellen.<br>"))
				    (get-menu-html))))
    (make-html-site html-string)))

(defun do-user-login ()
  (if (login-valid-p (hunchentoot:parameter "username")
		     (hunchentoot:parameter "password"))
      (progn
	(setf (hunchentoot:session-value 'username)
	      (hunchentoot:parameter "username"))
	(setf (hunchentoot:session-value 'userid)
	      (get-userid (hunchentoot:parameter "username")))
	(setf (hunchentoot:session-value 'own-userid)
	      (get-userid (hunchentoot:parameter "username")))
	(main-content))
      (make-html-site (concatenate 'string
				   "Loginversuch erfolglos<br><br><br>"
				   (get-login-html)))))

(defun show-last ()
  (main-content))

(defun day-profile ()
  (make-html-site
   (concatenate 'string
		"<h2>das durchschnittliche Tagesprofil ist:</h2>"
		"<table><tr><td>Zeit</td><td>Durchschnittswert</td></tr>"
		(let ((table-string "") (result (day-statistics
			       (hunchentoot:session-value 'userid)
			       (parse-integer (hunchentoot:parameter "start")
					      :junk-allowed t)
			       (parse-integer (hunchentoot:parameter "stop")
					      :junk-allowed t))))
		  (dolist (current result)
		    (setf table-string (concatenate 'string table-string
				 "<tr><td>"
				 (format () "~a Uhr"
					 (getf current :|hour(zeit)|))
				 "</td><td>"
				 (get-string (getf current :|avg(value)|))
				 "</td></tr>")))
		  table-string)
		"</table><br><br>"
		(get-menu-html))))

(defun value-interval()
  (make-html-site (concatenate 'string "<h3>die Messwerte sind:</h3>"
			       (make-main-table (hunchentoot:session-value
						 'userid)
						(values-from-interval
						 (hunchentoot:session-value
						  'userid)
						 (parse-integer
						  (hunchentoot:parameter
						   "start") :junk-allowed t)
						 (parse-integer
						  (hunchentoot:parameter
						  "stop") :junk-allowed t)))
			       (get-menu-html))))

(defun week-profile ()
  (make-html-site
   (concatenate 'string
		"<h2>das durchschnittliche Wochenprofil ist:</h2>"
		"<table><tr><td>Wochentag</td><td>Stunde</td>"
		"<td>Durchschnittswert</td></tr>"
		(let ((table-string "") (result (week-statistics
			       (hunchentoot:session-value 'userid)
			       (parse-integer (hunchentoot:parameter "start")
					      :junk-allowed t)
			       (parse-integer (hunchentoot:parameter "stop")
					      :junk-allowed t))))
		  (dolist (current result)
		    (setf table-string (concatenate 'string table-string
				 "<tr><td>"
				 (let ((day
					(getf current :|weekday(zeit)|)))
				   (cond
				     ((= day 0) "Montag")
				     ((= day 1) "Dienstag")
				     ((= day 2) "Mittwoch")
				     ((= day 3) "Donnerstag")
				     ((= day 4) "Freitag")
				     ((= day 5) "Samstag")
				     ((= day 6) "Sonntag")))))
		    (setf table-string (concatenate 'string table-string	    				 "</td><td>"	    
				 (format () "~a Uhr"
					 (getf current :|hour(zeit)|))
				 "</td><td>"
				 (get-string (getf current :|avg(value)|))
				 "</td></tr>")))
		  table-string)
		"</table><br><br>"
		(get-menu-html))))

(defun get-statistics-site ()
  (let ((data (first (get-statistics
	       (hunchentoot:session-value 'userid)
	       (parse-integer (hunchentoot:parameter "start") :junk-allowed t)
	       (parse-integer (hunchentoot:parameter "stop")
			      :junk-allowed t)))))
    (make-html-site (concatenate 'string "<h2>Statistische Auswertung:"
				 "</h2>Mittelwert: "
				 (get-string (getf data :|avg(value)|))
				 " Maximalwert: "
				 (get-string (getf data :|max(value)|))
				 " Minimalwert: "
				 (get-string (getf data :|min(value)|))
				 " Standardabweichung: "
				 (get-string (getf data :|stddev_pop(value)|))
				 " Anzahl der Messerte: "
				 (format () "~a"
					 (getf data :|count(value)|))
				 "<br><br><br>"
				 (get-menu-html)))))

(defun get-conditions ()
  (concatenate 'string "Um diesen Webservice zu nutzen, muss man"
			       " den Nutzungsbedingungen zustimmen.<br><br>"
			       "Die Bedingungen sind folgende:<br><br>"
			       "1. Diese Seite erhebt speichert und "
			       "verarbeitet personenbezogene Daten. "
			       "Z. B. Blutzuckermesswerte und die "
			       "Kommentare, die eingegeben werden.<br><br>"
			       "2. Diese Seite kann Cookies setzen. "
			       "Diese Cookies dienen dazu die Funktion "
			       "der Seite zu erm&ouml;glichen.<br><br>"
			       "3. Der Betreiber der Seite ist kein Experte "
			       "f&uuml;r Sicherheit Programmierung oder "
			       "Webserver. Daten die hier eingegeben werden,"
			       " m&uuml;ssen deswegen als verloren oder "
			       "gestohlen betrachtet werden. Jede/r Nutzer/ "
			       "Nutzerin muss seine/ihre Daten selbst "
			       "sichern.<br><br>"
			       "4. Der Betreiber der Seite hat das Recht "
			       "jederzeit, ohne Angabe von Gr&uuml;nden "
			       "Zug&auml;nge zu sperren oder zu l&ouml;schen."
			       " Ja auch aus reiner Willk&uuml;r.<br><br>"
			       "5. Kontaktdaten des Betreibers sind "
			       "<a href=?op=kontakt>hier</a> zu finden."
			       "<br><br>"
			       "6. Wenn man dem Betreiber nicht traut, oder "
			       "es besser kann und deswegen"
			       "seine eigene Seite Betreiben will kann man "
			       "das gerne tun. "
			       "<a href=https://github.com/moredhel0/"
			       "diab/blob/master/diab.lisp>"
			       "Hier gibt es den Quellcode</a>.<br><br>"
			       "7. Dem Betreiber ist klar, dass er keine "
			       "Ahnung von Webdesign hat. Und die Seite "
			       "deswegen h&auml;sslich ist.<br>"
			       "Wer es besser kann, siehe 6. "
			       "wenn sich jemand deswegen beschweren "
			       "will siehe 4.<br><br><br>"))

(defun new-user ()
  (make-html-site (concatenate 'string "<h2>Herzlich willkommen</h2>"
			       (get-conditions)
			       "<form method=post action=?op=accept>"
			       "<input type=submit value=\"Bedingungen "
			       "akzeptieren\"></form>")))

(defun get-new-user-site ()
  (concatenate 'string "Bitte die Daten des/der neuen Benutzers/Benutzerin "
	       "eintragen.<br><form method=post action=?op=createuser>"
	       "<table><tr><td>Ein Nnutzername f&uuml;r den Login</td>"
	       "<td><input type=text name=username></td></tr>"
	       "<tr><td>E-Mail-Adresse</td><td>"
	       "<input type=text name=mail></td></tr>"
	       "<tr><td>Passwort</td><td>"
	       "<input type=password name=pass></td></tr>"
	       "<tr><td>Passwort wiederholen</td><td>"
	       "<input type=password name=pass2></td></tr></table>"
	       "<br>Tabelle mit Messwerten anlegen:<br>"
	       "<input type=radio name=make value=ja>ja<br>"
	       "<input type=radio name=make value=nein>nein<br><br>"
	       "Einheit f&uuml;r die Blutzuckermesswerte:<br>"
	       "<input type=radio name=bz value=mg>mg/dl<br>"
	       "<input type=radio name=bz value=mmol>mmol/l<br><br>"
	       "Einheit f&uuml;r die Mahlzeiten:<br>"
	       "<input type=radio name=kh value=be>BE<br>"
	       "<input type=radio name=kh value=khe>KHE<br>"
	       "<input type=radio name=kh value=g>Gramm Kohlenhydrate<br><br>"
	       "<input type=submit value=\"Neues Konto anlegen\"></form>"
	       "<br>Hinweise f&uuml;r ein sicheres Passwort kann man "
	       "<a href=\"https://xkcd.com/936/\">hier</a> finden.<br><br>"))

(defun accepted-conditions ()
  (make-html-site (get-new-user-site)))

(defun value-submitted-p (value)
  (and value (not (string-equal value ""))))

(defun get-medi-list ()
  (let ((return-string "<h2>Medikamente</h2>")
	(med-list (hunchentoot:session-value 'med-list)))
    (setf return-string (concatenate 'string
				     return-string
				     "Bitte die Namen und Einheiten "
				     "(i.E. mg, Tabletten oder &Auml;hnliches)"
				     " eingeben.<br><br>"))
    (if med-list
	(progn (setf return-string (concatenate 'string return-string
					 "bisher eingegebene Medikamente:<br>"
					 "<table><tr><td>Name</td>"
					 "<td>Einheit</td></tr>"))
	       (do () ((not med-list))
		 (setf return-string (concatenate
				      'string
				      return-string
				      "<tr><td>"
				      (get-string (first med-list))
				      "</td><td>"
				      (get-string (first (rest med-list)))
				      "</td></tr>"))
		 (setf med-list (rest (rest med-list))))
	       (setf return-string (concatenate
				    'string
				    return-string
				    "</table><br>"
				    "<form method=post action=?op=meddone>"
				    "<input type=submit value="
				    "\"Diese Medikamente verwenden\">"
				    "</form><br>")))
	(setf return-string (concatenate 'string
					 return-string
					 "<form method=post "
					 "action=?op=meddone>"
					 "<input type=submit value=\""
					 "ohne Medikamente Weitermachen\">"
					 "</form><br>")))
    (setf return-string (concatenate 'string
				     return-string
				     "<h3>zus&auml;tzliches Medikament "
				     "eingeben.</h3>"
				     "<table><tr><td>"
				     "<form method=post action=?op=addmed>"
				     "Name:</td><td>"
				     "<input type=text name=medname></td></tr>"
				     "<tr><td>Einheit:</td>"
				     "<td><input type=text name=unit></td>"
				     "</tr></table>"
				     "<br><input type=submit value=hinzufügen>"
				     "</form>"))
    return-string))

(defun add-med-site ()
  (setf (hunchentoot:session-value 'med-list)
	(append (hunchentoot:session-value 'med-list)
		(list (hunchentoot:parameter "medname")
		      (hunchentoot:parameter "unit"))))
  (make-html-site (get-medi-list)))

(defun create-user ()
  (let ((new-user (hunchentoot:parameter "username"))
	(pass1 (hunchentoot:parameter "pass"))
	(pass2 (hunchentoot:parameter "pass2"))
	(mail (hunchentoot:parameter "mail"))
	(make (hunchentoot:parameter "make"))
	(bz (hunchentoot:parameter "bz"))
	(kh (hunchentoot:parameter "kh")))
    (make-html-site
     (cond
       ((user-exists-p new-user)
	(concatenate 'string "<h3>Nutzername nicht verf&uuml;gbar</h3>"
		     (get-new-user-site)))
       ((not (value-submitted-p new-user))
	(concatenate 'string "<h3>kein Nutzername angegeben</h3>"
		     (get-new-user-site)))
       ((not (string= pass1 pass2))
       (concatenate 'string "<h3>Passwort und Wiederholung sind "
			  "verschieden</h3>"
			  (get-new-user-site)))
       ((not (value-submitted-p pass1))
	(concatenate 'string "<h3>kein Passwort angegeben</h3>"
		     (get-new-user-site)))
       ((not (value-submitted-p mail))
	(concatenate 'string "<h3>keine E-Mail-Adresse angegeben</h3>"
		     (get-new-user-site)))
       ((not (value-submitted-p make))
	(concatenate 'string "<h3>nicht ausgew&auml;hlt, ob eigene Messwerte"
		     " gespeichert werden sollen</h3>"
		     (get-new-user-site)))
       ((and (string-equal make "ja") (not (value-submitted-p bz)))
	(concatenate 'string "<h3>Wenn eine Tabelle angelegt wird muss"
		     " auch eine Einheit f&uuml;r den Blutzucker angegeben "
		     "werden</h3>"
		     (get-new-user-site)))
       ((and (string-equal make "ja") (not (value-submitted-p kh)))
	(concatenate 'string "<h3>Wenn eine Tabelle angelegt wird, muss "
		     "auch eine Einheit f&uuml;r die Kohlenhydrate "
		     "angegeben werden</h3>"
		     (get-new-user-site)))
       (T (add-new-user new-user mail pass1) (if (string-equal make "ja")
						 (progn
						   (setf
						    (hunchentoot:session-value
						     'bz) bz)
						   (setf
						    (hunchentoot:session-value
						     'username) new-user)
						   (setf
						    (hunchentoot:session-value
						     'kh) kh)
						   (setf
						    (hunchentoot:session-value
						     'med-list) ())
						   (get-medi-list))
						 (concatenate
						  'string
						  "<h3>Nutzer angelegt</h3>"
						  (get-login-html))))))))

(defun med-done ()
  (create-user-tables (hunchentoot:session-value 'username)
		      (hunchentoot:session-value 'bz)
		      (hunchentoot:session-value 'kh)
		      (hunchentoot:session-value 'med-list))
  (make-html-site (concatenate 'string "<h2>Benutzer wurde angelegt</h2>"
			       (get-login-html))))

(defun process-calls (op)
  (if (not op)
      (let ((in (open "diab.config" :if-does-not-exist nil)))
	(if (not in)
	    (do-installation)
	    (progn
	      (close in)
	      (show-login-form))))
      (cond
	((string-equal op "login") (do-user-login))
	((string-equal op "showlast") (show-last))
	((string-equal op "daystat") (day-profile))
	((string-equal op "weekstat") (week-profile))
	((string-equal op "value-interval") (value-interval))
	((string-equal op "stat") (get-statistics-site))
	((string-equal op "newuser") (new-user))
	((string-equal op "accept") (accepted-conditions))
	((string-equal op "createuser") (create-user))
	((string-equal op "addmed") (add-med-site))
	((string-equal op "meddone") (med-done))
	)))

(defun start-server (&optional (port 8181))
  (hunchentoot:define-easy-handler (diab :uri "/") (op) (process-calls op))
   (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor
				     :port port)))

