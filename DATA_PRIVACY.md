# Data Privacy

This page explains, in simple terms, how the CytokineProfile app handles uploaded data while you use it. It is meant to be a practical summary, not a formal legal privacy policy.

## What You Upload

You may upload your own cytokine data files to analyze them in the app. The app currently accepts:

- `.csv`
- `.txt`
- `.xls`
- `.xlsx`

You may also choose one of the app's built-in example datasets instead of uploading your own file.

## How Uploaded Data Is Handled

Your uploaded data is used only inside your active app session so you can preview it, filter it, edit it, and run analyses.

The app also keeps the choices you make during the workflow in memory while the session is active.

When the app needs a working copy of an uploaded file or a built-in example dataset, it places that copy in a temporary app session folder.

## Temporary Storage During App Use

The app now keeps temporary files separated by session:

- uploaded files and cached built-in datasets are stored in a temporary folder for your session
- one session's temporary files are kept separate from another session's temporary files
- if you upload a replacement file during the same session, the older temporary copy for that slot is removed and replaced with the new one

This helps avoid shared leftover files across sessions while still allowing features like Excel sheet selection, data editing, and built-in dataset use during one session.
The app is set up to delete that temporary app session folder when the session ends. If a session does not close cleanly, old leftover session folders are removed the next time the app starts a new session, once they are more than 24 hours old.

## Good Practices for Sensitive Data

If possible, avoid uploading directly identifying, regulated, or otherwise highly sensitive data.

Good practices include:

- removing direct identifiers before upload
- using de-identified or coded study data when possible
- limiting uploads to only the variables needed for the analysis
- confirming your institution's data-governance requirements before use

## Questions

If you are unsure whether a dataset is appropriate to upload, or if you need stricter retention or cleanup rules for a deployment, contact the app maintainer before using sensitive data.
