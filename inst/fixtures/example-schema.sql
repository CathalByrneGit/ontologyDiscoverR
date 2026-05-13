-- Hospital Domain SQL DDL
-- Example schema for ontologyDiscoverR fixtures

CREATE TABLE patients (
    patient_id   SERIAL PRIMARY KEY,
    first_name   VARCHAR(100) NOT NULL,
    last_name    VARCHAR(100) NOT NULL,
    date_of_birth DATE        NOT NULL,
    gender       VARCHAR(10),
    email        VARCHAR(200),
    phone        VARCHAR(20),
    created_at   TIMESTAMP DEFAULT NOW()
);

CREATE TABLE wards (
    ward_id    SERIAL PRIMARY KEY,
    name       VARCHAR(100) NOT NULL,
    capacity   INTEGER      NOT NULL,
    floor      INTEGER,
    department VARCHAR(100)
);

CREATE TABLE doctors (
    doctor_id      SERIAL PRIMARY KEY,
    first_name     VARCHAR(100) NOT NULL,
    last_name      VARCHAR(100) NOT NULL,
    specialisation VARCHAR(100),
    license_number VARCHAR(50)  NOT NULL UNIQUE
);

CREATE TABLE admissions (
    admission_id    SERIAL PRIMARY KEY,
    patient_id      INTEGER   NOT NULL REFERENCES patients(patient_id),
    ward_id         INTEGER   NOT NULL REFERENCES wards(ward_id),
    admitted_at     TIMESTAMP NOT NULL,
    discharged_at   TIMESTAMP,
    diagnosis       TEXT,
    length_of_stay  INTEGER
);

CREATE TABLE appointments (
    appointment_id SERIAL PRIMARY KEY,
    patient_id     INTEGER   NOT NULL REFERENCES patients(patient_id),
    doctor_id      INTEGER   NOT NULL REFERENCES doctors(doctor_id),
    scheduled_at   TIMESTAMP NOT NULL,
    status         VARCHAR(20) DEFAULT 'scheduled',
    notes          TEXT
);

CREATE TABLE medications (
    medication_id SERIAL PRIMARY KEY,
    name          VARCHAR(200) NOT NULL,
    dosage        VARCHAR(100),
    frequency     VARCHAR(50)
);

CREATE TABLE prescriptions (
    prescription_id SERIAL PRIMARY KEY,
    admission_id    INTEGER      NOT NULL REFERENCES admissions(admission_id),
    medication_id   INTEGER      NOT NULL REFERENCES medications(medication_id),
    prescribed_at   TIMESTAMP    NOT NULL,
    dose_mg         NUMERIC(10,2)
);
