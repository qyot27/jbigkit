Older versions of jbig.c contained a list of human-readable
result/error messages in both English and German.

These messages are highly technical and unlikely to be of much use to
end users who are not familiar with the JBIG1 standard (which has only
been published in English). Therefore, in the interest of simplicity,
the current source code contains in errmsg[] now only the English
version of these messages.

The German version is preserved here in the form of a PO translation
file, as used by the GNU gettext package, just in case anyone still
finds it of use.

See http://www.gnu.org/software/gettext/manual/gettext.html for
information on the PO file format and tools for creating and using
them.
