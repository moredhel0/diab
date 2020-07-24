(require 'cl-dbi)
(require 'cl-base64)
(require 'ironclad)
(require 'cl-smtp)
(require 'local-time)
(require 'hunchentoot)
(require 'cl-ppcre)

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
    (print (list :name name :mail email :address address) out)))

(defun write-own-url (url)
  (with-open-file (out "url.config" :direction :output
		       :if-exists :supersede)
    (format out "~a~%" ";this is an automatically generated file")
    (format out "~a~%" ";do only edit it if you know what you are doing")
    (print (list :url url) out)))

(defun read-own-url ()
  (with-open-file (in "url.config" :direction :input)
    (read in)))

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

(defun is-base64-p (input-string)
  (if (cl-ppcre:scan "^([A-Za-z0-9+/]{4})*([A-Za-z0-9+/]{4}|[A-Za-z0-9+/]{3}=|[A-Za-z0-9+/]{2}==)?$" input-string)
      T
      ()))

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
  (let (result (connection (get-connection (read-config))) return-string)
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
	      (dbi:do-sql connection "delete from pwrenew where username = ?"
			  (list (getf result :|username|)))
	      (setf return-string "Neues Passwort gesetzt"))
	    (setf return-string "Kein g&uuml;ltiger Code mehr vorhanden."))
	(setf return-string "Kein g&uuml;ltiger Code gfunden."))
    (dbi:disconnect connection)
    return-string))
    
(defun change-password (userid new-password)
  (let ((connection (get-connection (read-config))))
    (dbi:do-sql connection "update users set password = ? where id = ?"
		(list (encode (ironclad:pbkdf2-hash-password-to-combined-string
			       (ironclad:ascii-string-to-byte-array
				new-password)))
		      userid))
    (dbi:disconnect connection)))

(defun delete-entry (userid dataset-id)
  (let ((connection (get-connection (read-config))))
    (dbi:do-sql connection "delete from sugar_values? where id = ?"
      (list userid dataset-id))
    (dbi:disconnect connection)))

(defun insert-entry (userid timestamp value food remark medication)
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
    (userid entryid timestamp value food remark  medication)
  (let ((connection (get-connection (read-config)))
	(query-string
	 "update sugar_values? set zeit=?, value=?, food=?, remark=?")
	(parameters (list userid timestamp value food (encode remark)))
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
    
(defun delete-user (userid)
  (let ((connection (get-connection (read-config))) username)
    (setf username (get-username userid))
    (dbi:do-sql connection "drop table if exists sugar_values?" (list userid))
    (dbi:do-sql connection "drop table if exists medi?" (list userid))
    (dbi:do-sql connection "delete from accesslevels where username = ?"
		(list (encode username)))
    (dbi:do-sql connection "delete from users where id = ?" (list userid))
    (dbi:disconnect connection)))

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

(defun can-write-p ()
  (if (= (hunchentoot:session-value 'userid)
	 (hunchentoot:session-value 'own-userid))
      T
      (has-write-priv-p (get-username (hunchentoot:session-value 'own-userid))
			(hunchentoot:session-value 'userid))))

(defun has-read-priv-p (username target-userid)
  (if (get-max-privlevel username target-userid)
      T
      nil))

(defun can-read-p ()
  (if (= (hunchentoot:session-value 'userid)
	 (hunchentoot:session-value 'own-userid))
      T
      (has-read-priv-p (get-username (hunchentoot:session-value 'own-userid))
			(hunchentoot:session-value 'userid))))

(defun has-owner-priv-p (username target-userid)
  (string-equal (get-max-privlevel username target-userid) "o"))

(defun user-base64-exists-p (username)
  (if (get-query-results "select * from users where name=?"
			 (list username))
      t
      ()))

(defun user-exists-p (username)
  (user-base64-exists-p (encode username)))

(defun value-submitted-p (value)
  (and value (not (string-equal value ""))))

(defun make-html-site (content)
  (setf (hunchentoot:content-type*) "text/html")
  (concatenate 'string
	       "<!doctype html>"
	       "<head><title>Blutzuckermesswerte Webservice</title>"
	       "</head><body>"
	       content
	       "<br><br>"
	       "<img src=\"https://moredhel.is-a-geek.net/lisp.jpg\" "
	       "alt=\"made with Lisp\"></img></body></html>"))

(defun get-installation-html (text db-user db-pass db-name db-port db-server
			      contact-name contact-mail contact-address
			      url mail-server send-mail-address mail-username
			      mail-pass mail-port mail-encryption)
  (make-html-site (concatenate 'string
			       text
			       "<h2>Webservice einrichten</h2>"
			       "<br><br>Wichtig, wenn die Daten nicht "
			       "korrekt eingetragen werden, funktioniert "
			       "der Webservice nicht. Die Daten k&ouml;nnen "
			       "dann nur durch Bearbeiten, oder L&ouml;schen "
			       "der .config Dateien ge&auml;ndert werden.<br>"
			       "<br><br>"
			       "<form method=post "
			       "action=\"?op=initial-install\">"
			       "<Table><tr><td>Daten zum Webserver:"
			       "</td><td></td></tr><tr><td>"
			       "Url der Seite eingeben</td><td>"
			       "<input type=text name=\"url\" "
			       (if url
				   (concatenate 'string "value=\"" url "\""))
			       "></td></tr><tr><td></td><td></td></tr><tr>"
			       "<td>Kontaktdaten des Benutzers:</td><td></td>"
			       "</tr><tr><td>Name:</td><td><input type=text "
			       "name=\"contact-name\" "
			       (if contact-name
				   (concatenate 'string "value=\"" contact-name
						"\""))
			       "></td></tr><tr><td>E-Mail f&uuml;r die "
			       "Kontaktseite</td><td><input type=text name="
			       "\"contact-mail\" "
			       (if contact-mail
				   (concatenate 'string "value=\""
						contact-mail
						"\""))
			       "></td></tr><tr><td>Zus&auml;tzliche "
			       "Adressdaten f&uuml;r die Kontaktseite</td><td>"
			       "<input type=text name=\"contact-address\" "
			       (if contact-address
				   (concatenate 'string
						"value=\"" contact-address
						"\""))
			       "></td></tr><tr><td></td><td></td></tr><tr><td>"
			       "Daten zum Mailkonto &uuml;ber das die Seite "
			       "Nachrichten verschicken kann:</td><td></td>"
			       "</tr><tr><td>E-Mail-Adresse</td><td><input "
			       "type=text name=\"send-mail-address\" "
			       (if send-mail-address
				   (concatenate 'string
						"value=\"" send-mail-address
						"\""))
			       "></td></tr><tr><td>E-Mail-Server</td><td>"
			       "<input type=text name=\"mail-server\" "
			       (if mail-server
				   (concatenate 'string
						"value=\"" mail-server "\""))
			       "></td></tr><tr><td>Username f&uumlr den "
			       "Mailserver</td><td><input type=text name=\""
			       "mail-username\" "
			       (if mail-username
				   (concatenate 'string
						"value=\"" mail-username "\""))
			       "></td></tr><tr><td>Passwort</td><td>"
			       "<input type=password name=\"mail-pass\" "
			       (if mail-pass
				   (concatenate 'string
						"value=\"" mail-pass "\""))
			       "></td></tr><tr><td>Port (optional)</td><td>"
			       "<input type=text name=\"mail-port\" "
			       (if mail-port
				   (concatenate 'string "value=\""
						mail-port
						"\""))
			       "></td></tr><tr><td>Verschl&uuml;sselung</td>"
			       "<td><input type=radio name=\"mail-encryption\" "
			       "value=\"tls\" "
			       (if (string-equal mail-encryption "tls")
				   "checked")
			       ">tls<input type=radio name=\""
			       "mail-encryption\" value=\"starttls\" "
			       (if (string-equal mail-encryption "starttls")
				   "checked")
			       ">starttls"
			       "<input type=radio name=\"mail-encryption\" "
			       "value=\"none\" "
			       (if (string-equal mail-encryption "none")
				   "checked")
			       ">keine</td></tr><tr><td></td>"
			       "<td></td></tr>"
			       "<tr><td>Daten f&uuml;r die Datenbankverbindung"
			       "</td><td></td></tr><tr><td>Benutzername "
			       "f&uuml;r die Datenbank.</td><td><input type="
			       "text name=\"db-user\" "
			       (if db-user
				   (concatenate 'string
						"value=\"" db-user "\""))
			       "></td></tr><tr><td>Datenbankpasswort</td><td>"
			       "<input type=password name=\"db-pass\" "
			       (if db-pass
				   (concatenate 'string
						"value=\"" db-pass "\""))
			       "></td></tr><tr><td>Datenbankname</td><td><input"
			       " type=text name=\"db-name\" "
			       (if db-name
				   (concatenate 'string
						"value=\"" db-name "\""))
			       "></td></tr><tr><td>Port (optional)</td><td>"
			       "<input type=text name=\"db-port\" "
			       (if db-port
				   (concatenate 'string
						"value=\"" db-port "\""))
			       "></td></tr><tr><td>Datenbankserver</td><td>"
			       "<input type=text name=\"db-server\" value=\""
			       (if db-server
				   db-server
				   "localhost")
			       "\"></td></tr><tr><td><input type=submit "
			       "value=\"Einrichten\"></td><td></td></tr>"
			       "</table></form>")))

(defun test-db-data ( db-name db-user db-pass db-server &optional db-port )
  (let (connection (return-value))
    (handler-case 
    (progn (setf connection (if db-port
			 (dbi:connect :mysql :database-name db-name
				      :username db-user :password db-pass
				      :host db-server :port db-port)
			 (dbi:connect :mysql :database-name db-name
				      :username db-user :password db-pass
				      :host db-server)))
    (if (dbi:fetch (dbi:execute (dbi:prepare connection "select 1 + 2")))
	(setf return-value T))
    (dbi:disconnect connection))
      (error () ()))
    return-value))

(defun test-mail-data (mail-address mail-server mail-user mail-pass
		       mail-encryption &optional mail-port)
  (if mail-port
      (write-mailconfig mail-server mail-address mail-user mail-pass mail-port
			(if (string-equal mail-encryption "starttls")
			    :starttls
			    (if (string-equal mail-encryption "tls")
				:tls)))
      (write-mailconfig mail-server mail-address mail-user mail-pass 25
			(if (string-equal mail-encryption "starttls")
			    :starttls
			    (if (string-equal mail-encryption "tls")
				:tls))))
  (if (handler-case (send-message mail-address "Testnachricht"
				  "Test der E-Mail-einstellungen")
	(error () ()))
      T
      ()))

(defun do-installation ()
  (get-installation-html "" () () () () () () () () () () () () () () ()))

(defun do-initial-install ()
  (let ((url (hunchentoot:parameter "url"))
	(contact-name (hunchentoot:parameter "contact-name"))
	(contact-mail (hunchentoot:parameter "contact-mail"))
	(contact-address (hunchentoot:parameter "contact-address"))
	(send-mail-address (hunchentoot:parameter "send-mail-address"))
	(mail-server (hunchentoot:parameter "mail-server"))
	(mail-username (hunchentoot:parameter "mail-username"))
	(mail-pass (hunchentoot:parameter "mail-pass"))
	(mail-port (hunchentoot:parameter "mail-port"))
	(mail-encryption (hunchentoot:parameter "mail-encryption"))
	(db-user (hunchentoot:parameter "db-user"))
	(db-pass (hunchentoot:parameter "db-pass"))
	(db-name (hunchentoot:parameter "db-name"))
	(db-port (hunchentoot:parameter "db-port"))
	(db-server (hunchentoot:parameter "db-server"))
	(in (open "diab.config" :if-does-not-exist nil)))
    (if (not in)
    (cond
      ((not (value-submitted-p url))
      (get-installation-html "<h2>Url wurde nicht angegeben</h2>"
			     db-user db-pass db-name db-port db-server
			     contact-name contact-mail contact-address url
			     mail-server send-mail-address mail-username
			     mail-pass mail-port mail-encryption))
      ((not (value-submitted-p contact-name))
      (get-installation-html "<h2>kein Name f&uuml;r die Kontaktdaten</h2>"
			     db-user db-pass db-name db-port db-server
			     contact-name contact-mail contact-address url
			     mail-server send-mail-address mail-username
			     mail-pass mail-port mail-encryption))
      ((not (value-submitted-p contact-mail))
      (get-installation-html "<h2>keine E-Mail f&uuml;r Kontakdaten</h2>"
			     db-user db-pass db-name db-port db-server
			     contact-name contact-mail contact-address url
			     mail-server send-mail-address mail-username
			     mail-pass mail-port mail-encryption))
      ((not (value-submitted-p send-mail-address))
      (get-installation-html "<h2>E-Mail f&uuml;r Versand nicht angegeben</h2>"
			     db-user db-pass db-name db-port db-server
			     contact-name contact-mail contact-address url
			     mail-server send-mail-address mail-username
			     mail-pass mail-port mail-encryption))
      ((not (value-submitted-p mail-server))
      (get-installation-html "<h2>Mailserver wurde nicht angegeben</h2>"
			     db-user db-pass db-name db-port db-server
			     contact-name contact-mail contact-address url
			     mail-server send-mail-address mail-username
			     mail-pass mail-port mail-encryption))
      ((not (value-submitted-p mail-username))
      (get-installation-html "<h2>E-Mail Nutzername wurde nicht angegeben</h2>"
			     db-user db-pass db-name db-port db-server
			     contact-name contact-mail contact-address url
			     mail-server send-mail-address mail-username
			     mail-pass mail-port mail-encryption))
      ((not (value-submitted-p mail-pass))
      (get-installation-html "<h2>E-Mail-Passwort wurde nicht angegeben</h2>"
			     db-user db-pass db-name db-port db-server
			     contact-name contact-mail contact-address url
			     mail-server send-mail-address mail-username
			     mail-pass mail-port mail-encryption))
      ((not (value-submitted-p mail-encryption))
      (get-installation-html "<h2>keine Verschl&uuml;sselung angegeben</h2>"
			     db-user db-pass db-name db-port db-server
			     contact-name contact-mail contact-address url
			     mail-server send-mail-address mail-username
			     mail-pass mail-port mail-encryption))
      ((not (value-submitted-p db-user))
      (get-installation-html "<h2>kein Datenbanknutzername angegeben</h2>"
			     db-user db-pass db-name db-port db-server
			     contact-name contact-mail contact-address url
			     mail-server send-mail-address mail-username
			     mail-pass mail-port mail-encryption))
      ((not (value-submitted-p db-pass))
      (get-installation-html "<h2>kein Datenbankpasswort angegeben</h2>"
			     db-user db-pass db-name db-port db-server
			     contact-name contact-mail contact-address url
			     mail-server send-mail-address mail-username
			     mail-pass mail-port mail-encryption))
      ((not (value-submitted-p db-name))
      (get-installation-html "<h2>kein Datenbankname angegeben</h2>"
			     db-user db-pass db-name db-port db-server
			     contact-name contact-mail contact-address url
			     mail-server send-mail-address mail-username
			     mail-pass mail-port mail-encryption))
      ((not (value-submitted-p db-server))
      (get-installation-html "<h2>kein Datenbankserver angegeben</h2>"
			     db-user db-pass db-name db-port db-server
			     contact-name contact-mail contact-address url
			     mail-server send-mail-address mail-username
			     mail-pass mail-port mail-encryption))
      ((not (if (value-submitted-p db-port)
		(test-db-data db-name db-user db-pass db-server db-port)
		(test-db-data db-name db-user db-pass db-server)))
       (get-installation-html (concatenate 'string "<h2>Datenbankverbindung"
					   " funktioniert nicht</h2>Bitte alle"
					   " Daten pr&uuml;fen")
			     db-user db-pass db-name db-port db-server
			     contact-name contact-mail contact-address url
			     mail-server send-mail-address mail-username
			     mail-pass mail-port mail-encryption))
      ((not (if (value-submitted-p mail-port)
		(test-mail-data send-mail-address mail-server mail-username
				mail-pass mail-encryption mail-port)
		(test-mail-data send-mail-address mail-server mail-username
				mail-pass mail-encryption)))
	    (get-installation-html (concatenate 'string "<h2>E-Mail Versand"
					   " funktioniert nicht</h2>Bitte alle"
					   " Daten pr&uuml;fen")
			     db-user db-pass db-name db-port db-server
			     contact-name contact-mail contact-address url
			     mail-server send-mail-address mail-username
			     mail-pass mail-port mail-encryption))
      (T (write-contact-data contact-name contact-mail contact-address)
	 (write-own-url url)
	 (write-config db-user db-pass db-name
		       (if (value-submitted-p db-port) db-port nil)
		       (if (value-submitted-p db-server) db-server nil))
	 (install-basic-tables)
	 (make-html-site (concatenate 'string "Installation erfolgreich<br>"
				      "<a href=\"" url "\">hier</a> geht "
				      "es weiter"))))
    (close in))))

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

(defun make-ro-main-table (userid values-list)
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
	  (concatenate 'string return-string "</tr>"))
    (dolist (current values-list)
      (setf return-string
	    (concatenate 'string return-string
			 "<tr><td>"
			 (get-date-string (getf current :|zeit|))
			 "</td><td>"
			 (get-string (getf current :|value|))
			 "</td><td>"
			 (get-string (getf current :|food|))
			 "</td><td>"
			 (decode (getf current :|remark|))
			 "</td>"))
        (dotimes (i (length medi-list))
	(setf return-string (concatenate 'string return-string "<td>"
					 (get-string (nth (+ 11 (* 2 i))
							  current)) "</td>")))
      (setf return-string
	    (concatenate 'string
			 return-string
			 "</tr><br>")))))

(defun get-menu-html ()
  (let ((return-string (concatenate 'string "Daten von Nutzer "
				    (hunchentoot:session-value 'username)
				    "<br><br><br>")))
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
			(get-username (hunchentoot:session-value
				       'own-userid)))))
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
		 "<a href=\"?op=deleteuser\">Eigenes Benutzerkonto "
		 "l&ouml;schen</a><br>"
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
		       (if (can-write-p)
			   (make-main-table
			    (hunchentoot:session-value 'userid)
			    (get-last-values
			     (hunchentoot:session-value 'userid)
			     (parse-integer value-count :junk-allowed t)))
			   (make-ro-main-table
			    (hunchentoot:session-value 'userid)
			    (get-last-values
			     (hunchentoot:session-value 'userid)
			     (parse-integer value-count :junk-allowed t)))
			   )
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
			       "Hier gibt es die Software</a>.<br><br>"
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
    (setf (hunchentoot:session-value 'username) new-user)
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

(defun terms ()
  (make-html-site (concatenate 'string (get-conditions) (get-menu-html))))

(defun kontakt ()
  (let ((data (read-contact-data)))
    (make-html-site (concatenate 'string
				 "<h2>Kontaktdaten des Betreibers:</h2>"
				 "<br><br>"
				 (getf data :name)
				 "<br><a href=\"mailto:"
				 (getf data :mail)
				 "\">"
				 (getf data :mail)
				 "</a><br><br>"
				 (getf data :address)
				 "<br><br>"
				 (if (hunchentoot:session-value 'userid)
				     (get-menu-html))))))

(defun relogin ()
  (make-html-site (concatenate 'string
			       "<h2>Login abgelaufen</h2>"
			       (get-login-html))))

(defun make-password-reset-html ()
  (concatenate 'string
	       "Hier k&ouml;nnen sie ein neues Passwort anfordern.<br>"
	       "Bitte dafür den Nutzernamen eingeben. Es wird ein Link "
	       "per E-Mail verschickt, mit dem ein neues Passwort gesetzt "
	       "werden kann.<br><br>"
	       "Wenn keine g&uuml;ltige Adresse hinterlegt ist, Pech gehabt."
	       "<br><br>Der Link bleibt nur 5 Stunden g&uuml;ltig.<br><br>"
	       "Bitte den Nutzernamen eingeben, um den es geht:"
	       "<form method=\"post\" action=\"?op=dopwreset\">"
	       "<input type=\"text\" name=\"username\">"
	       "<input type=submit value=\"Reset anfordern\"></form>"))

(defun get-mail (username)
  (decode (getf (first (get-query-results
			"select email from users where name=?"
		     (list (encode username)))) :|email|)))

(defun do-pw-reset ()
  (let ((username (hunchentoot:parameter "username")) (user-mail) (code) (url))
    (if (or (not username) (string-equal username ""))
	(make-html-site (concatenate 'string
				     "<h3>kein Nutzername angegeben</h3>"
				     (make-password-reset-html)))
	(progn
	  (setf user-mail (get-mail username))
	  (if (not (string-equal user-mail ""))
	      (progn
	      (setf url (getf (read-own-url) :url))
	      (setf code (format () "~a" (request-password-reset username)))
	      (send-message user-mail "Passwortanforderung"
			    (concatenate
			     'string
			     "Um ein neues Passwort zu setzen, bitte "
			     "folgenden Link anklicken."
			     "<a href=\"" url
			     "?op=setnewpw&code="
			     code "\">" url "?op=setnewpw&code=" code
			     "</a>."))))
	  (make-html-site "E-Mail wurde verschickt")))))

(defun demand-password-reset ()
  (make-html-site
   (concatenate 'string "<h2>Passwort vergessen</h2>"
		(make-password-reset-html))))

(defun new-pw-site ()
  (make-html-site
   (concatenate 'string
		"bitte neues Passwort eingeben"
		"<form method=post action=\"?op=doresetpw\">"
		"<input type=hidden name=\"code\" value="
		(hunchentoot:get-parameter "code")
		"><input type=\"password\" name=\"newpw\">"
		"<input type=submit value=\"Passwort setzen\">"
		"</form>")))

(defun reset-pw ()
  (make-html-site
   (concatenate
    'string
    (reset-password (hunchentoot:parameter "code")
		    (hunchentoot:parameter "newpw"))
    "<br><br>"
    (get-login-html))))

(defun confirm-delete ()
  (make-html-site (concatenate
		   'string
		   "<h2>Ihr Konto l&ouml;schen</h2>"
		   "<br><br>"
		   "Wichtig!<br>Wenn das Konto gel&ouml;scht wird, "
		   "Sind ALLE Daten UNWIEDERBRINGLICH verloren.<br>"
		   "Es kann NICHTS wiederhergestellt werden.<br><br>"
		   "<form method=post action=\"?op=dodelete\">"
		   "<input type=submit value=\"verstanden trotzdem "
		   "l&ouml;schen\"></form>")))

(defun do-delete ()
  (delete-user (hunchentoot:session-value 'own-userid))
  (make-html-site (concatenate 'string
			       "Konto wurde gel&ouml;scht.")))

(defun change-password-form-html ()
  (concatenate 'string
	       "<h2>Passwort &auml;ndern</h2>"
	       "<form method=post action=\"?op=dochpw\">"
	       "<table><tr><td>Bisheriges Passwort:</td>"
	       "<td><input type=password name=oldpw></td></tr>"
	       "<tr><td>neues Passwort</td><td>"
	       "<input type=password name=newpw></td></tr>"
	       "<tr><td>neues Passwort wiederholen</td>"
	       "<td><input type=password name=newpw2></td>"
	       "</tr><tr><td></td><td>"
	       "<input type=submit value=\"neues Passwort speichern\">"
	       "</td></tr></table></form>"
	       "<br><br>"
	       (get-menu-html)))

(defun change-password-form ()
  (make-html-site (change-password-form-html)))

(defun do-change-pw ()
  (let ((oldpass (hunchentoot:parameter "oldpw"))
	(newpass (hunchentoot:parameter "newpw"))
	(newpass2 (hunchentoot:parameter "newpw2")))
    (make-html-site
     (cond
       ((not (value-submitted-p oldpass))
	(concatenate 'string "Bisheriges Passwort nicht angegeben<br>"
		     (change-password-form-html)))
       ((not (string= newpass newpass2))
	(concatenate 'string
		     "Passwort und Wiederholung sind nicht identisch<br>"
		     (change-password-form-html)))
       ((not (value-submitted-p newpass))
	(concatenate 'string "es muss ein neues Passwort angegeben werden<br>"
		     (change-password-form-html)))
       ((not (ironclad:pbkdf2-check-password
	      (ironclad:ascii-string-to-byte-array oldpass)
	      (decode (getf (first (get-query-results
				    "select password from users where id = ?"
				    (list (hunchentoot:session-value
					   'own-userid)))) :|password|))))
	(concatenate 'string "bisheriges Passwort ung&uuml;ltig"
		     (change-password-form-html)))
       (T (change-password (hunchentoot:session-value 'own-userid) newpass)
	  (concatenate 'string "Passwort ge&auml;ndert<br><br>"
		       (get-menu-html)))))))

(defun priv-error-site ()
  (make-html-site (concatenate 'string "<h2>Fehlende berechtigungen</h2>"
			       "<br><br>Entweder fehlen die "
			       "Zugriffsberechtigungen, oder es "
			       "ist ein Fehler aufgetreten.")))

(defun do-delete-entry ()
  (if (can-write-p)
      (progn
	(delete-entry (hunchentoot:session-value 'userid)
		      (hunchentoot:parameter "id"))
	(show-last))
      (priv-error-site)))

(defun test-string (input)
  (if (and input (not (string= input "")))
      input
      nil))

(defun get-med-number ()
  (getf (first (get-query-results "select count(id) from medi?"
				  (list (hunchentoot:session-value 'userid))))
	:|count(id)|))

(defun get-med-parameters ()
  (let ((return-list ()) (number (get-med-number)))
    (dotimes (i number)
      (setf return-list
	    (append return-list
		    (list (test-string (hunchentoot:parameter
			   (concatenate 'string "med"
					(format () "~a" i))))))))
    return-list))

(defun is-date-time-p (date)
  (if (local-time:parse-timestring date :date-time-separator #\space
				   :fail-on-error ())
      T
      ()))

(defun do-add-entry ()
  (if (can-write-p)
      (progn
	(if (is-date-time-p (hunchentoot:parameter "time"))
	    (progn
	      (insert-entry
	       (hunchentoot:session-value 'userid)
	       (test-string (hunchentoot:parameter "time"))
	       (test-string (hunchentoot:parameter "sugar"))
	       (test-string (hunchentoot:parameter "food"))
	       (hunchentoot:parameter "remark")
	       (get-med-parameters))
	       (show-last))
	    (make-html-site (concatenate 'string
					 "Die Zeitangabe war nicht im Format:"
					 " Jahr-Monat-Tag "
					 "Stunde:Minute:Sekunde<br><br>"
					 (get-menu-html)))))
      (priv-error-site)))

(defun do-change-entry ()
  (if (can-write-p)
      (progn
	(if (is-date-time-p (hunchentoot:parameter "time"))
	    (progn
	      (change-entry
	       (hunchentoot:session-value 'userid)
	       (hunchentoot:parameter "entry")
	       (test-string (hunchentoot:parameter "time"))
	       (test-string (hunchentoot:parameter "sugar"))
	       (test-string (hunchentoot:parameter "food"))
	       (hunchentoot:parameter "remark")
	       (get-med-parameters))
	       (show-last))
	    (make-html-site (concatenate 'string
					 "Die Zeitangabe war nicht im Format:"
					 " Jahr-Monat-Tag "
					 "Stunde:Minute:Sekunde<br><br>"
					 (get-menu-html)))))
      (priv-error-site)))

(defun priv-exists-p (target-username)
  (if (get-query-results
       (concatenate 'string "select * from accesslevels where "
		    "username=? and tablename='sugar_values?'")
       (list (encode target-username) (hunchentoot:session-value 'own-userid)))
      T
      nil))

(defun get-foreign-accessible-lists ()
  (get-query-results
   (concatenate 'string "select * from accesslevels where "
		"tablename='sugar_values?' and not level='o'")
   (list (hunchentoot:session-value 'own-userid))))

(defun add-priv (base64-encoded-username privlevel)
  (let ((connection (get-connection (read-config))))
    (dbi:do-sql connection (concatenate 'string "insert into accesslevels "
					"(username, tablename,level) values "
					"(?, 'sugar_values?', ?)")
		(list base64-encoded-username
		      (hunchentoot:session-value 'own-userid) privlevel))
    (dbi:disconnect connection)))

(defun revoke-priv (base64-encoded-username)
  (let ((connection (get-connection (read-config))))
    (dbi:do-sql connection (concatenate 'string "delete from accesslevels "
					"where username=? and "
					"tablename='sugar_values?'")
		(list base64-encoded-username
		      (hunchentoot:session-value 'own-userid)))
    (dbi:disconnect connection)))

(defun change-priv (base64-encoded-username privlevel)
  (let ((connection (get-connection (read-config))))
    (dbi:do-sql connection
      (concatenate 'string "update accesslevels set level=? where "
		   "username=? and tablename='sugar_values?'")
      (list privlevel base64-encoded-username
	    (hunchentoot:session-value 'own-userid)))
    (dbi:disconnect connection)))

(defun priv-html ()
  (let ((privs (get-foreign-accessible-lists))
	(return-string "<h2>Berechtigungen verwalten</h2>"))
    (if privs
	(progn
	(setf return-string (concatenate 'string
					 return-string
					 "<table>"))
	
	(dolist (current privs)
	  (setf return-string (concatenate
			       'string return-string
			       "<tr><td>Nutzername: </td>"
			       "<td>"(decode (getf current :|username|))"</td>"
			       "<form method=post action=\"?op=chpriv\">"
			       "<input type=hidden name=user value=\""
			       (getf current :|username|)
			       "\"><td>"
			       (if (string-equal (getf current :|level|) "w")
				   (concatenate
				    'string
				    "<input type=radio name=level value=\"w\" "
				    "checked>lesen und schreiben</td>"
				    "<td><input type=radio name=level "
				    "value=\"r\">lesen</td>")
				   (concatenate
				    'string
				    "<input type=radio name=level value=\"w\">"
				    "lesen und schreiben</td>"
				    "<td><input type=radio name=level "
				    "value=\"r\" checked>lesen</td>"))
			       "<td><input type=submit value="
			       "\"Berechtigunen &auml;ndern\"></td></form>"
			       "<td><form method=post action=\"?op=rmpriv\">"
			       "<input type=hidden name=user value=\""
			       (getf current :|username|)
			       "\"><input type=submit value=\"Berechtigungen "
			       "l&ouml;schen\"></form></td></tr>")))
	(setf return-string (concatenate 'string
					 return-string
					 "</table>"))))
	(setf return-string (concatenate 'string return-string
	       "<br><form method=post action=\"?op=newpriv\">"
	       "Username: <input type=text name=\"user\"> "
	       "Welche Berechtigung soll gegeben werden?"
	       "<input type=radio name=\"priv\" value=\"r\">nur lesen "
	       "<input type=radio name=\"priv\" value=\"w\">"
	       "lesen und schreiben."
	       "<input type=submit value=\"Rechte speichern\">"
	       "</form><br><br>"))
  return-string))

(defun priv-site ()
  (make-html-site (concatenate 'string (priv-html)(get-menu-html))))

(defun do-add-priv ()
  (let ((user (hunchentoot:parameter "user"))
	(level (hunchentoot:parameter "priv")))
    (cond
      ((not (value-submitted-p level))
       (make-html-site
	(concatenate 'string
		     "<h3>Kein Berechtigungslevel angegeben</h3><br>"
		     (priv-html)(get-menu-html))))
      ((not (value-submitted-p user))
       (make-html-site
	(concatenate 'string
		     "<h3>Kein Benutzername angegeben</h3><br>"
		     (priv-html)(get-menu-html))))
      ((priv-exists-p user)
	(make-html-site (concatenate
			 'string
			 "<h3>dieser Nutzer hat bereits Zugriffsrechte. "
			 "Hinzuf&uuml;gen ist deswegen nicht m&ouml;glich. "
			 "Bearbeiten geht.</h3><br>"
			 (priv-html)(get-menu-html))))
      ((not (user-exists-p user))
       (make-html-site (concatenate 'string
				    "<h3>Nutzer existiert nicht</h3><br>"
				    (priv-html)(get-menu-html))))
      (T (add-priv (encode user) level)(priv-site)))))

(defun do-del-priv ()
  (revoke-priv (hunchentoot:parameter "user"))
  (priv-site))

(defun do-change-priv ()
  (change-priv (hunchentoot:parameter "user") (hunchentoot:parameter "level"))
  (priv-site))

(defun change-view ()
  (setf (hunchentoot:session-value 'userid)
	(parse-integer (hunchentoot:parameter "uid") :junk-allowed t))
  (if (can-read-p)
      (progn (setf (hunchentoot:session-value 'username)
		   (get-username (hunchentoot:session-value 'userid)))
	     (main-content))
      (progn
	(setf (hunchentoot:session-value 'userid)
	      (hunchentoot:session-value 'own-userid))
	(priv-error-site))))

(defun make-text-return (content)
  (setf (hunchentoot:content-type*) "text/plain")
  content)

(defun text-create-user (user pass)
  (cond
    ((not (value-submitted-p (hunchentoot:parameter "email")))
     (make-text-return "email needed"))
    ((user-base64-exists-p user) (make-text-return "user exists"))
    (t (add-new-user (decode user) (hunchentoot:parameter "email")
		     (decode pass))
       (make-text-return "ok"))))

(defun get-userid-base64 (username)
  (getf (first (get-query-results "select id from users where name=?"
				  (list username))) :|id|))

(defun text-delete-user (username)
  (delete-user (get-userid-base64 username))
  (make-text-return "ok"))

(defun text-add-table (username)
  (let ((sugar-unit (hunchentoot:parameter "sugarUnit"))
	(food-unit (hunchentoot:parameter "foodUnit"))
	(medlist nil))
    (cond
      ((not (value-submitted-p sugar-unit))
       (make-text-return "sugar unit needed"))
      ((not (or (string-equal sugar-unit "mmol")
		(string-equal sugar-unit "mg")))
       (make-text-return "unknown sugar unit"))
      ((not (value-submitted-p food-unit))
       (make-text-return "food unit needed"))
      ((not (or (string-equal food-unit "be")
		(string-equal food-unit "khe")
		(string-equal food-unit "g")))
       (make-text-return "unknown food unit"))
      (t (do
       ((i 1 (1+ i)))
       ((not (value-submitted-p (hunchentoot:parameter (format () "med~a" i)))))
	   (setf medlist (append medlist (list
					  (hunchentoot:parameter
					   (format () "med~a" i))
					  (hunchentoot:parameter
					   (format () "medunit~a" i))))))
	 (create-user-tables (decode username) sugar-unit food-unit medlist)
	 (make-text-return "ok")))))

(defun text-interface ()
  (let ((parameter (hunchentoot:parameter "do"))
	(user (hunchentoot:parameter "username"))
	(pass (hunchentoot:parameter "pass")))
    (cond
      ((not (value-submitted-p parameter)) (make-text-return "no action given"))
      ((string-equal parameter "protocolVersion") (make-text-return
						   "mor diab protocol version "
						   "1.0"))
      ((not (value-submitted-p user)) (make-text-return "username needed"))
      ((not (value-submitted-p pass)) (make-text-return "pass needed"))
      ((not (is-base64-p user)) (make-text-return "username not base64"))
      ((not (is-base64-p pass)) (make-text-return "pass not base64"))
      ((string-equal parameter "createUser") (text-create-user user pass))
      ;;;;login needed
      ((not (login-valid-p (decode user) (decode pass)))
       (make-text-return "login incorrect"))
      ((string-equal parameter "deleteUser") (text-delete-user user))
      ((string-equal parameter "createTable") (text-add-table user))
      (T (make-text-return "unknown action")))))

(defun invalid-op ()
  (make-html-site (concatenate 'string
			       "<h2>Etwas uerwartetes ist passiert</h2>"
			       "Unbekannte Anfrage. Bearbeitung nicht "
			       "m&ouml;glich")))

(defun make-table-html ()
  (concatenate 'string
	       "<h3>Tabelle Anlegen</h3>"
	       "<form method=post action=\"?op=setunits\">"
	       "<table><tr><td>Einheit f&uuml;r den Blutzucker:</td><td>"
	       "<input type=radio name=\"bz\" value=\"mg\">"
	       "mg/dl <input type=radio name=\"bz\" value=\"mol\">mmol/l</td>"
	       "</tr><tr><td>Einheit f&uuml;r die Kohlenhydrate</td><td>"
	       "<input type=radio name=\"be\" value=\"be\">BE "
	       "<input type=radio name=\"be\" value=\"khe\">KHE "
	       "<input type=radio name=\"be\" value=\"g\">Gramm</td></tr>"
	       "<tr><td><input type=submit value=\"Einheiten festlegen\"></td>"
	       "<td></td></tr></table></form>"
	       (get-menu-html)))

(defun make-table ()
  (setf (hunchentoot:session-value 'med-list) ())
  (make-html-site (make-table-html)))

(defun make-med-html ()
  (let ((return-string "<h2>Medikamente</h2>") (medlist (hunchentoot)))
    (setf return-string (concatenate 'string return-string ""))
    return-string))

(defun set-units ()
  (let ((be (hunchentoot:parameter "be"))(bz (hunchentoot:parameter "bz")))
    (cond
      ((not (value-submitted-p bz))
       (make-html-site
	 (concatenate 'string
		      "Keine Einheit f&uuml;r den Blutzucker angegeben"
		      (make-table-html))))
      ((not (value-submitted-p be))
       (make-html-site
	 (concatenate 'string
		      "Keine Einheit f&uuml;r Kohlenhydrate angegeben"
		      (make-table-html))))
      (T (setf (hunchentoot:session-value 'username)
	       (get-username (hunchentoot:session-value 'own-userid)))
	 (setf (hunchentoot:session-value 'bz) bz)
	 (setf (hunchentoot:session-value 'kh) be)
	 (make-html-site (get-medi-list))))))


(defun process-calls (op)
  (if (not op)
      (let ((in (open "diab.config" :if-does-not-exist nil)))
	(if (not in)
	    (do-installation)
	    (progn
	      (close in)
	      (show-login-form))))
      (cond
	;;;; no login needed
	((string-equal op "login") (do-user-login))
	((string-equal op "newuser") (new-user))
	((string-equal op "accept") (accepted-conditions))
	((string-equal op "createuser") (create-user))
	((string-equal op "terms") (terms))
	((string-equal op "kontakt") (kontakt))
	((string-equal op "resetpw") (demand-password-reset))
	((string-equal op "dopwreset") (do-pw-reset))
	((string-equal op "setnewpw") (new-pw-site))
	((string-equal op "doresetpw") (reset-pw))
	((string-equal op "initial-install") (do-initial-install))
	((string-equal op "addmed") (add-med-site))
	((string-equal op "meddone") (med-done))
	((string-equal op "text") (text-interface))
	;;;; session expired -> relogin
	((not (hunchentoot:session-value 'userid)) (relogin))
	;;;; login is needed
	((string-equal op "showlast") (show-last))
	((string-equal op "daystat") (day-profile))
	((string-equal op "weekstat") (week-profile))
	((string-equal op "value-interval") (value-interval))
	((string-equal op "stat") (get-statistics-site))
	((string-equal op "deleteuser") (confirm-delete))
	((string-equal op "dodelete") (do-delete))
	((string-equal op "changepw") (change-password-form))
	((string-equal op "dochpw") (do-change-pw))
	((string-equal op "delete") (do-delete-entry))
	((string-equal op "addnew") (do-add-entry))
	((string-equal op "change") (do-change-entry))
	((string-equal op "givepriv") (priv-site))
	((string-equal op "newpriv") (do-add-priv))
	((string-equal op "rmpriv") (do-del-priv))
	((string-equal op "chpriv") (do-change-priv))
	((string-equal op "chuser") (change-view))
	((string-equal op "maketable") (make-table))
	((string-equal op "setunits") (set-units))
	(T (invalid-op)))))

(defun start-server (&optional (port 8181))
  (hunchentoot:define-easy-handler (diab :uri "/") (op) (process-calls op))
   (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor
				     :port port)))

