// To parse this data:
//
//   import { Convert, APITypes } from "./file";
//
//   const aPITypes = Convert.toAPITypes(json);
//
// These functions will throw an error if the JSON doesn't
// match the expected interface, even if the JSON is valid.

export interface APITypes {
    info:       Info;
    paths:      Paths;
    components: Components;
    openapi:    string;
}

export interface Components {
    schemas: Schemas;
}

export interface Schemas {
    User:   User;
    UserId: SchemasUserID;
}

export interface User {
    properties: Properties;
    required:   string[];
    type:       string;
}

export interface Properties {
    userId:   UserIDClass;
    userName: UserName;
}

export interface UserIDClass {
    $ref: string;
}

export interface UserName {
    type: string;
}

export interface SchemasUserID {
    maximum: number;
    minimum: number;
    type:    string;
}

export interface Info {
    title:   string;
    version: string;
}

export interface Paths {
    "/api/users":      APIUsers;
    "/api/users/{id}": APIUsersID;
}

export interface APIUsers {
    get:  Get;
    post: Post;
}

export interface Get {
    responses: Responses;
}

export interface Responses {
    "200": The200;
}

export interface The200 {
    content:     The200_Content;
    description: string;
}

export interface The200_Content {
    "application/json;charset=utf-8": PurpleApplicationJSONCharsetUTF8;
}

export interface PurpleApplicationJSONCharsetUTF8 {
    schema: PurpleSchema;
}

export interface PurpleSchema {
    items: UserIDClass;
    type:  string;
}

export interface Post {
    requestBody: RequestBody;
    responses:   { [key: string]: PostResponse };
}

export interface RequestBody {
    content: RequestBodyContent;
}

export interface RequestBodyContent {
    "application/json;charset=utf-8": FluffyApplicationJSONCharsetUTF8;
}

export interface FluffyApplicationJSONCharsetUTF8 {
    schema: UserIDClass;
}

export interface PostResponse {
    content?:    RequestBodyContent;
    description: string;
}

export interface APIUsersID {
    put:    Put;
    delete: Delete;
}

export interface Delete {
    parameters: Parameter[];
    responses:  { [key: string]: DeleteResponse };
}

export interface Parameter {
    in:       string;
    name:     string;
    required: boolean;
    schema:   SchemasUserID;
}

export interface DeleteResponse {
    content?:    PurpleContent;
    description: string;
}

export interface PurpleContent {
    "application/json;charset=utf-8": TentacledApplicationJSONCharsetUTF8;
}

export interface TentacledApplicationJSONCharsetUTF8 {
    schema: FluffySchema;
}

export interface FluffySchema {
    example:  any[];
    items:    Items;
    maxItems: number;
    type:     string;
}

export interface Items {
}

export interface Put {
    parameters:  Parameter[];
    requestBody: RequestBody;
    responses:   { [key: string]: DeleteResponse };
}

// Converts JSON strings to/from your types
// and asserts the results of JSON.parse at runtime
export class Convert {
    public static toAPITypes(json: string): APITypes {
        return cast(JSON.parse(json), r("APITypes"));
    }

    public static aPITypesToJson(value: APITypes): string {
        return JSON.stringify(uncast(value, r("APITypes")), null, 2);
    }
}

function invalidValue(typ: any, val: any, key: any, parent: any = ''): never {
    const prettyTyp = prettyTypeName(typ);
    const parentText = parent ? ` on ${parent}` : '';
    const keyText = key ? ` for key "${key}"` : '';
    throw Error(`Invalid value${keyText}${parentText}. Expected ${prettyTyp} but got ${JSON.stringify(val)}`);
}

function prettyTypeName(typ: any): string {
    if (Array.isArray(typ)) {
        if (typ.length === 2 && typ[0] === undefined) {
            return `an optional ${prettyTypeName(typ[1])}`;
        } else {
            return `one of [${typ.map(a => { return prettyTypeName(a); }).join(", ")}]`;
        }
    } else if (typeof typ === "object" && typ.literal !== undefined) {
        return typ.literal;
    } else {
        return typeof typ;
    }
}

function jsonToJSProps(typ: any): any {
    if (typ.jsonToJS === undefined) {
        const map: any = {};
        typ.props.forEach((p: any) => map[p.json] = { key: p.js, typ: p.typ });
        typ.jsonToJS = map;
    }
    return typ.jsonToJS;
}

function jsToJSONProps(typ: any): any {
    if (typ.jsToJSON === undefined) {
        const map: any = {};
        typ.props.forEach((p: any) => map[p.js] = { key: p.json, typ: p.typ });
        typ.jsToJSON = map;
    }
    return typ.jsToJSON;
}

function transform(val: any, typ: any, getProps: any, key: any = '', parent: any = ''): any {
    function transformPrimitive(typ: string, val: any): any {
        if (typeof typ === typeof val) return val;
        return invalidValue(typ, val, key, parent);
    }

    function transformUnion(typs: any[], val: any): any {
        // val must validate against one typ in typs
        const l = typs.length;
        for (let i = 0; i < l; i++) {
            const typ = typs[i];
            try {
                return transform(val, typ, getProps);
            } catch (_) {}
        }
        return invalidValue(typs, val, key, parent);
    }

    function transformEnum(cases: string[], val: any): any {
        if (cases.indexOf(val) !== -1) return val;
        return invalidValue(cases.map(a => { return l(a); }), val, key, parent);
    }

    function transformArray(typ: any, val: any): any {
        // val must be an array with no invalid elements
        if (!Array.isArray(val)) return invalidValue(l("array"), val, key, parent);
        return val.map(el => transform(el, typ, getProps));
    }

    function transformDate(val: any): any {
        if (val === null) {
            return null;
        }
        const d = new Date(val);
        if (isNaN(d.valueOf())) {
            return invalidValue(l("Date"), val, key, parent);
        }
        return d;
    }

    function transformObject(props: { [k: string]: any }, additional: any, val: any): any {
        if (val === null || typeof val !== "object" || Array.isArray(val)) {
            return invalidValue(l(ref || "object"), val, key, parent);
        }
        const result: any = {};
        Object.getOwnPropertyNames(props).forEach(key => {
            const prop = props[key];
            const v = Object.prototype.hasOwnProperty.call(val, key) ? val[key] : undefined;
            result[prop.key] = transform(v, prop.typ, getProps, key, ref);
        });
        Object.getOwnPropertyNames(val).forEach(key => {
            if (!Object.prototype.hasOwnProperty.call(props, key)) {
                result[key] = transform(val[key], additional, getProps, key, ref);
            }
        });
        return result;
    }

    if (typ === "any") return val;
    if (typ === null) {
        if (val === null) return val;
        return invalidValue(typ, val, key, parent);
    }
    if (typ === false) return invalidValue(typ, val, key, parent);
    let ref: any = undefined;
    while (typeof typ === "object" && typ.ref !== undefined) {
        ref = typ.ref;
        typ = typeMap[typ.ref];
    }
    if (Array.isArray(typ)) return transformEnum(typ, val);
    if (typeof typ === "object") {
        return typ.hasOwnProperty("unionMembers") ? transformUnion(typ.unionMembers, val)
            : typ.hasOwnProperty("arrayItems")    ? transformArray(typ.arrayItems, val)
            : typ.hasOwnProperty("props")         ? transformObject(getProps(typ), typ.additional, val)
            : invalidValue(typ, val, key, parent);
    }
    // Numbers can be parsed by Date but shouldn't be.
    if (typ === Date && typeof val !== "number") return transformDate(val);
    return transformPrimitive(typ, val);
}

function cast<T>(val: any, typ: any): T {
    return transform(val, typ, jsonToJSProps);
}

function uncast<T>(val: T, typ: any): any {
    return transform(val, typ, jsToJSONProps);
}

function l(typ: any) {
    return { literal: typ };
}

function a(typ: any) {
    return { arrayItems: typ };
}

function u(...typs: any[]) {
    return { unionMembers: typs };
}

function o(props: any[], additional: any) {
    return { props, additional };
}

function m(additional: any) {
    return { props: [], additional };
}

function r(name: string) {
    return { ref: name };
}

const typeMap: any = {
    "APITypes": o([
        { json: "info", js: "info", typ: r("Info") },
        { json: "paths", js: "paths", typ: r("Paths") },
        { json: "components", js: "components", typ: r("Components") },
        { json: "openapi", js: "openapi", typ: "" },
    ], false),
    "Components": o([
        { json: "schemas", js: "schemas", typ: r("Schemas") },
    ], false),
    "Schemas": o([
        { json: "User", js: "User", typ: r("User") },
        { json: "UserId", js: "UserId", typ: r("SchemasUserID") },
    ], false),
    "User": o([
        { json: "properties", js: "properties", typ: r("Properties") },
        { json: "required", js: "required", typ: a("") },
        { json: "type", js: "type", typ: "" },
    ], false),
    "Properties": o([
        { json: "userId", js: "userId", typ: r("UserIDClass") },
        { json: "userName", js: "userName", typ: r("UserName") },
    ], false),
    "UserIDClass": o([
        { json: "$ref", js: "$ref", typ: "" },
    ], false),
    "UserName": o([
        { json: "type", js: "type", typ: "" },
    ], false),
    "SchemasUserID": o([
        { json: "maximum", js: "maximum", typ: 0 },
        { json: "minimum", js: "minimum", typ: 0 },
        { json: "type", js: "type", typ: "" },
    ], false),
    "Info": o([
        { json: "title", js: "title", typ: "" },
        { json: "version", js: "version", typ: "" },
    ], false),
    "Paths": o([
        { json: "/api/users", js: "/api/users", typ: r("APIUsers") },
        { json: "/api/users/{id}", js: "/api/users/{id}", typ: r("APIUsersID") },
    ], false),
    "APIUsers": o([
        { json: "get", js: "get", typ: r("Get") },
        { json: "post", js: "post", typ: r("Post") },
    ], false),
    "Get": o([
        { json: "responses", js: "responses", typ: r("Responses") },
    ], false),
    "Responses": o([
        { json: "200", js: "200", typ: r("The200") },
    ], false),
    "The200": o([
        { json: "content", js: "content", typ: r("The200_Content") },
        { json: "description", js: "description", typ: "" },
    ], false),
    "The200_Content": o([
        { json: "application/json;charset=utf-8", js: "application/json;charset=utf-8", typ: r("PurpleApplicationJSONCharsetUTF8") },
    ], false),
    "PurpleApplicationJSONCharsetUTF8": o([
        { json: "schema", js: "schema", typ: r("PurpleSchema") },
    ], false),
    "PurpleSchema": o([
        { json: "items", js: "items", typ: r("UserIDClass") },
        { json: "type", js: "type", typ: "" },
    ], false),
    "Post": o([
        { json: "requestBody", js: "requestBody", typ: r("RequestBody") },
        { json: "responses", js: "responses", typ: m(r("PostResponse")) },
    ], false),
    "RequestBody": o([
        { json: "content", js: "content", typ: r("RequestBodyContent") },
    ], false),
    "RequestBodyContent": o([
        { json: "application/json;charset=utf-8", js: "application/json;charset=utf-8", typ: r("FluffyApplicationJSONCharsetUTF8") },
    ], false),
    "FluffyApplicationJSONCharsetUTF8": o([
        { json: "schema", js: "schema", typ: r("UserIDClass") },
    ], false),
    "PostResponse": o([
        { json: "content", js: "content", typ: u(undefined, r("RequestBodyContent")) },
        { json: "description", js: "description", typ: "" },
    ], false),
    "APIUsersID": o([
        { json: "put", js: "put", typ: r("Put") },
        { json: "delete", js: "delete", typ: r("Delete") },
    ], false),
    "Delete": o([
        { json: "parameters", js: "parameters", typ: a(r("Parameter")) },
        { json: "responses", js: "responses", typ: m(r("DeleteResponse")) },
    ], false),
    "Parameter": o([
        { json: "in", js: "in", typ: "" },
        { json: "name", js: "name", typ: "" },
        { json: "required", js: "required", typ: true },
        { json: "schema", js: "schema", typ: r("SchemasUserID") },
    ], false),
    "DeleteResponse": o([
        { json: "content", js: "content", typ: u(undefined, r("PurpleContent")) },
        { json: "description", js: "description", typ: "" },
    ], false),
    "PurpleContent": o([
        { json: "application/json;charset=utf-8", js: "application/json;charset=utf-8", typ: r("TentacledApplicationJSONCharsetUTF8") },
    ], false),
    "TentacledApplicationJSONCharsetUTF8": o([
        { json: "schema", js: "schema", typ: r("FluffySchema") },
    ], false),
    "FluffySchema": o([
        { json: "example", js: "example", typ: a("any") },
        { json: "items", js: "items", typ: r("Items") },
        { json: "maxItems", js: "maxItems", typ: 0 },
        { json: "type", js: "type", typ: "" },
    ], false),
    "Items": o([
    ], false),
    "Put": o([
        { json: "parameters", js: "parameters", typ: a(r("Parameter")) },
        { json: "requestBody", js: "requestBody", typ: r("RequestBody") },
        { json: "responses", js: "responses", typ: m(r("DeleteResponse")) },
    ], false),
};
