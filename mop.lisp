(in-package #:kyoto-persistence)

(defclass persistent-class (standard-class)
  ())

(defmethod validate-superclass ((class persistent-class) (super standard-class))
  "Persistent classes may inherit from ordinary classes."
  t)

(defclass indexed-slot-definition (standard-slot-definition)
  ((indexed :accessor indexed? :initarg :index :initform nil :allocation :instance)
   (transient :accessor transient? :initarg :transient :initform nil
              :allocation :instance)
   (unique :accessor unique? :initarg :unique :initform nil :allocation :instance)
   (key :accessor keyed? :initarg :key :initform nil :allocation :instance)
   (searchable :accessor searchable? :initarg :search :initform nil
               :allocation :instance)))

(defclass indexed-direct-slot-definition
    (standard-direct-slot-definition indexed-slot-definition)
  ())

(defclass indexed-effective-slot-definition
    (standard-effective-slot-definition indexed-slot-definition)
  ())

(defmethod indexed-slot-names ((instance persistent-class))
  "Return a list of indexed slot names for an instance."
  (map 'list #'sb-mop:slot-definition-name
       (remove-if-not #'(lambda (i)
                          (or (indexed? i) (unique? i) (keyed? i)))
                      (class-slots instance))))

(defmethod direct-slot-definition-class ((class persistent-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'indexed-direct-slot-definition))

(defmethod effective-slot-definition-class ((class persistent-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'indexed-effective-slot-definition))

(defmethod compute-effective-slot-definition :around
    ((class persistent-class) slot-name direct-slots)
  "Ensure inheritance from direct slot definition of indexed, revisioned, keyed,
   transient, unique properties. Create sequences for keyed slots and index tables
   for indexed, unique and keyed slots."
  (let ((slot (call-next-method))
        (unique-keys-p nil))
    (when (some #'keyed? direct-slots)
      (setf (slot-value slot 'key) t)
      (add-key-sequence class slot-name 'generate-uuid)
      (setf unique-keys-p t))
    (when (some #'unique? direct-slots)
      (setf (slot-value slot 'unique) t)
      (setf unique-keys-p t))
    (if (or (some #'indexed? direct-slots)
            (some #'keyed? direct-slots)
            (some #'unique? direct-slots))
        (progn
          (setf (slot-value slot 'indexed) t)
          (add-slot-index class slot-name :unique-keys-p unique-keys-p))
        (setf (slot-value slot 'indexed) nil))
    (setf (slot-value slot 'transient) (some #'transient? direct-slots))
    slot))

(defmacro def-persistent-class (name parents slot-defs &rest class-opts)
  `(progn
     (let ((class (defclass ,name ,parents ,slot-defs)))



(macroexpand-1 
'(def-persistent-class test ()
  ((uuid :accessor uuid :initarg :uuid :initform (make-uuid) :index t)
   (predicate :accessor predicate :initarg :predicate :initform nil :index t :index-key #'pred-name)
   (subject :accessor subject :initarg :subject :initform nil :index t :index-key #'node-value)
   (object :accessor object :initarg :object :initform nil :index t :index-key #'node-value)
   (timestamp :accessor timestamp :initarg :timestamp :initform (now))
   (belief-factor :accessor belief-factor :initarg :belief-factor :initform 1.0)
   (deleted? :accessor deleted? :initarg :deleted? :initform nil)
   (derived? :accessor derived? :initarg :derived? :initform nil)
   (spo-idx :index (predicate subject object))
   (sp-idx :index (subject predicate))
   (so-idx :index (subject object))
   (po-idx :index (predicate object))))
)
  