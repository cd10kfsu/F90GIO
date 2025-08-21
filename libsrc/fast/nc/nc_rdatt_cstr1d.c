#include <stdio.h>
#include <stdlib.h>
#include <netcdf.h>
#include <string.h>   

#define ERR(e) {printf("Error: %s\n", nc_strerror(e)); exit(1);}

// read the pointer of the string for netcdf string att
char *nc_rdatt_cstr1d(const int ncid, const char *varname, const char *attname, int *attlen) {
    int istat;

    size_t attlen_t = 0;
    int vid;

    if (0 == strlen(varname)) {
        vid = NC_GLOBAL; //global attribute
    } else {
        if ((istat = nc_inq_varid(ncid, varname, &vid))) ERR(istat);
    }
    if (( istat = nc_inq_attlen(ncid, vid, attname, &attlen_t) )) ERR(istat);
    if (attlen_t > 1) {
        printf("[error] read_att_string1d.c: only support 1d string (but actually %zu\n)",attlen_t);
        exit(2);
    }

    char **att_strings = (char**)malloc(attlen_t * sizeof(char*));
    memset(att_strings, 0, attlen_t * sizeof(char*));

    if ((istat = nc_get_att_string(ncid, vid, attname, att_strings))) ERR(istat);
    *attlen = strlen(att_strings[0]);
    char* attstr = (char*)malloc( (*attlen) * sizeof(char) + 1);
    strcpy(attstr, att_strings[0]);

    if (( istat = nc_free_string(attlen_t, att_strings) )) ERR(istat);
    free(att_strings);

    return attstr;
}

