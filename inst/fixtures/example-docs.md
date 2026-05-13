# Hospital Management System

## Overview

The Hospital Management System (HMS) is an integrated platform designed to manage the full lifecycle of patient care, from initial registration through admission, treatment, and discharge. The system consolidates information about patients, medical staff, wards, appointments, and medications into a single unified data model.

The HMS supports clinical and administrative workflows, providing real-time visibility into ward capacity, appointment scheduling, and patient status. It integrates with external laboratory, pharmacy, and billing systems via a RESTful API layer.

Key goals of the system:
- Centralise patient demographic and medical records
- Streamline ward allocation and bed management
- Facilitate appointment scheduling between patients and doctors
- Track admissions, diagnoses, and discharge readiness
- Support clinical decision-making through business rules and automated flags

## Patients

Patient registration is the entry point for all care pathways. Each patient record captures core demographic information including full name, date of birth, gender, contact details (email and phone), and a system-generated unique identifier.

**Registration:** New patients are registered either on walk-in, via GP referral, or through the online portal. At registration, mandatory fields include first name, last name, and date of birth. Contact details are captured where available.

**Demographics:** The system stores structured demographic data to support reporting, resource planning, and compliance with data protection regulations. Gender is recorded using a controlled vocabulary.

**Medical History:** The HMS maintains a longitudinal medical history for each patient, linking all admissions, appointments, diagnoses, prescriptions, and discharge summaries. Medical history is accessible to authorised clinical staff and forms the basis for risk stratification and care planning.

Patient records are never deleted; instead, they are flagged as inactive if a patient is deceased or has permanently left the region.

## Wards

The hospital is divided into wards, each serving a specific clinical function. Ward types include:

- **ICU (Intensive Care Unit):** High-dependency care for critically ill patients requiring continuous monitoring and life support. ICU wards have the lowest patient-to-nurse ratios and strict admission criteria.
- **General:** General medical and nursing care for patients who do not require specialist intervention. General wards handle the broadest range of conditions.
- **Surgical:** Pre- and post-operative care for patients undergoing surgical procedures. Surgical wards are co-located with operating theatres and recovery rooms.

**Capacity Management:** Each ward has a defined maximum capacity (number of beds). The system tracks current occupancy in real time and raises alerts when occupancy exceeds 85% of capacity. Ward managers can view live capacity dashboards and request temporary capacity increases subject to approval.

Each ward belongs to a hospital department (e.g., Cardiology, Orthopaedics, Emergency Medicine) and is located on a specific floor. Floor and department information is used for wayfinding and resource allocation.

## Admissions

The admission process begins when a clinician determines that a patient requires inpatient care. Admissions link a patient to a specific ward and record the date and time of admission.

**Admission Process:** A patient is admitted by creating an admission record that references both the patient and the receiving ward. The admitting doctor records an initial diagnosis, which may be updated throughout the stay. Ward allocation is subject to available bed capacity.

**Discharge Criteria:** A patient is considered ready for discharge when clinical criteria are met and administrative checks are complete. The system supports structured discharge checklists and requires a completed diagnosis entry before discharge can be finalised.

**Length of Stay (LOS):** Length of stay is calculated as the number of days between `admitted_at` and `discharged_at`. LOS is a key performance indicator. **Any admission with a length of stay greater than 3 days is automatically flagged for clinical review.** Extended stays may indicate complications, social care barriers, or delayed discharge.

Discharge summaries are generated automatically upon finalisation and sent to the patient's GP.

## Appointments

The appointments module manages scheduled consultations between patients and doctors.

**Scheduling:** Appointments are created by administrative staff or through the patient portal. Each appointment specifies a patient, a doctor, and a date/time. The system enforces doctor availability and prevents double-booking. Appointment status progresses through: `scheduled` → `completed` or `cancelled` or `no_show`.

**Cancellation Policy:** Appointments may be cancelled by either the patient or the hospital. Patient-initiated cancellations made less than 24 hours before the scheduled time are recorded as late cancellations. A patient with three or more late cancellations within a 12-month period is flagged for review. Hospital-initiated cancellations trigger an automatic rescheduling workflow.

Appointment history is retained indefinitely and contributes to the patient's longitudinal record.

## Business Rules

The HMS applies a set of business rules to automate clinical and operational decisions:

**High Risk Patient:** A patient is classified as *high risk* if they meet one or more of the following criteria:
- Age greater than 65 years (derived from `date_of_birth`)
- Has at least one previous admission recorded in the system (`previous_admissions > 0`)

High risk patients are flagged prominently in the patient record and are prioritised for clinical review during ward rounds.

**Ready for Discharge:** A patient is marked as *ready for discharge* when both of the following conditions are true:
- Length of stay is greater than 3 days (`length_of_stay > 3`)
- Diagnosis has been recorded and marked complete (`diagnosis_complete = TRUE`)

This rule drives the daily discharge planning workflow and is evaluated automatically each morning.

**Priority Appointment:** A patient is granted a *priority appointment* if they have had 2 or more previous admissions. Priority appointments are scheduled within 48 hours of request and are allocated dedicated slots in the doctor's timetable.

These business rules are implemented in the application layer and may also be expressed as SQL expressions for reporting and analytics purposes.
