(defclass <gears-database> (peer))

(defclass <gears-result-set> (peer))

(defun <gears-database> (&opt name)
  (let ((db (make <gears-database>)))
    (set (.peer db) {% google.gears.factory.create('beta.database') %})
    {% ~(.peer db).open(~name) %}
    db))

(defmethod gears-execute ((db <gears-database>) (stmt <string>) &rest args)
  (let ((rs-peer {% ~(.peer db).execute(~stmt, ~args) %}))
    (<gears-result-set> rs-peer)))

(defmethod gears-close ((db <gears-database>))
  {% ~(.peer db).close() %})

(defmethod gears-last-insert-row-id ((db <gears-database>))
  {% ~(.peer db).lastInsertRowId %})

(defmethod gears-rows-affected ((db <gears-database>))
  {% ~(.peer db).rowsAffected %})

(defun <gears-result-set> (peer)
  (let ((rs (make <gears-result-set>)))
    (set (.peer rs) peer)
    rs))

(defmethod gears-is-valid-row ((rs <gears-result-set>))
  {% ~(.peer rs).isValidRow() %})

(defmethod gears-next ((rs <gears-result-set>))
  {% ~(.peer rs).next() %})

(defmethod gears-close ((rs <gears-result-set>))
  {% ~(.peer rs).close() %})

(defmethod gears-field-count ((rs <gears-result-set>))
  {% ~(.peer rs).fieldCount() %})

(defmethod gears-field-name ((rs <gears-result-set>) (field-index <number>))
  {% ~(.peer rs).fieldName(~field-index) %})

(defmethod gears-field ((rs <gears-result-set>) (field-index <number>))
  {% ~(.peer rs).field(~field-index) %})

(defmethod gears-field-by-name ((rs <gears-result-set>) (field-name <string>))
  {% ~(.peer rs).fieldByName(~field-name) %})
