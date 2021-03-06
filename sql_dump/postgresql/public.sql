/*
 Navicat Premium Data Transfer

 Source Server         : ganymede local
 Source Server Type    : PostgreSQL
 Source Server Version : 80404
 Source Host           : localhost
 Source Database       : ganymede
 Source Schema         : public

 Target Server Type    : PostgreSQL
 Target Server Version : 80404
 File Encoding         : utf-8

 Date: 08/10/2010 01:04:44 AM
*/

-- ----------------------------
--  Sequence structure for "accounts_id_seq"
-- ----------------------------
DROP SEQUENCE IF EXISTS "accounts_id_seq";
CREATE SEQUENCE "accounts_id_seq" INCREMENT 1 START 22 MAXVALUE 9223372036854775807 MINVALUE 1 CACHE 1;
ALTER TABLE "accounts_id_seq" OWNER TO "postgres";

-- ----------------------------
--  Sequence structure for "rsc_metas_id_seq"
-- ----------------------------
DROP SEQUENCE IF EXISTS "rsc_metas_id_seq";
CREATE SEQUENCE "rsc_metas_id_seq" INCREMENT 1 START 18 MAXVALUE 9223372036854775807 MINVALUE 1 CACHE 1;
ALTER TABLE "rsc_metas_id_seq" OWNER TO "postgres";

-- ----------------------------
--  Sequence structure for "category_metas_id_seq"
-- ----------------------------
DROP SEQUENCE IF EXISTS "category_metas_id_seq";
CREATE SEQUENCE "category_metas_id_seq" INCREMENT 1 START 18 MAXVALUE 9223372036854775807 MINVALUE 1 CACHE 1;
ALTER TABLE "category_metas_id_seq" OWNER TO "postgres";

-- ----------------------------
--  Sequence structure for "nodes_id_seq"
-- ----------------------------
DROP SEQUENCE IF EXISTS "nodes_id_seq";
CREATE SEQUENCE "nodes_id_seq" INCREMENT 1 START 526151 MAXVALUE 9223372036854775807 MINVALUE 1 CACHE 1;
ALTER TABLE "nodes_id_seq" OWNER TO "postgres";

-- ----------------------------
--  Table structure for "accounts"
-- ----------------------------
DROP TABLE IF EXISTS "accounts";
CREATE TABLE "accounts" (
	"id" int8 NOT NULL DEFAULT nextval('accounts_id_seq'::regclass),
	"login" varchar(32) DEFAULT NULL::character varying,
	"person_id" varchar(32) DEFAULT NULL::character varying,
	"password" varchar(64) DEFAULT NULL::character varying,
	"name" varchar(32) DEFAULT NULL::character varying,
	"surname" varchar(32) DEFAULT NULL::character varying,
	"email" varchar(32) DEFAULT NULL::character varying,
	"description" text DEFAULT NULL,
	"role" int2 DEFAULT 0,
	"state" int2 DEFAULT 0,
	"reg_datetime" timestamp(6) NULL DEFAULT NULL::timestamp without time zone,
	"login_datetime" timestamp(6) NULL DEFAULT NULL::timestamp without time zone
)
WITH (OIDS=FALSE);
ALTER TABLE "accounts" OWNER TO "postgres";

-- ----------------------------
--  Table structure for "person_metas"
-- ----------------------------
DROP TABLE IF EXISTS "person_metas";
CREATE TABLE "person_metas" (
	"id" varchar NOT NULL DEFAULT NULL,
	"name" varchar DEFAULT NULL,
	"surname" varchar DEFAULT NULL
)
WITH (OIDS=FALSE);
ALTER TABLE "person_metas" OWNER TO "postgres";

-- ----------------------------
--  Table structure for "rsc_metas"
-- ----------------------------
DROP TABLE IF EXISTS "rsc_metas";
CREATE TABLE "rsc_metas" (
	"id" int8 NOT NULL DEFAULT nextval('rsc_metas_id_seq'::regclass),
	"name" varchar(64) DEFAULT NULL,
	"author_id" int8 DEFAULT NULL,
	"publisher_id" int8 DEFAULT NULL,
	"year" int2 DEFAULT NULL,
	"pages_count" int2 DEFAULT NULL,
	"abstract" varchar DEFAULT NULL,
	"discipline" varchar DEFAULT NULL
)
WITH (OIDS=FALSE);
ALTER TABLE "rsc_metas" OWNER TO "postgres";

-- ----------------------------
--  Table structure for "category_metas"
-- ----------------------------
DROP TABLE IF EXISTS "category_metas";
CREATE TABLE "category_metas" (
	"id" int8 NOT NULL DEFAULT nextval('category_metas_id_seq'::regclass),
	"name" varchar(64) DEFAULT NULL,
	"discipline" varchar DEFAULT NULL,
	"description" varchar DEFAULT NULL
)
WITH (OIDS=FALSE);
ALTER TABLE "category_metas" OWNER TO "postgres";

-- ----------------------------
--  Table structure for "nodes"
-- ----------------------------
DROP TABLE IF EXISTS "nodes";
CREATE TABLE "nodes" (
	"id" int8 NOT NULL DEFAULT nextval('nodes_id_seq'::regclass),
	"name" varchar(64) DEFAULT NULL::character varying,
	"type" int2 DEFAULT 0,
	"parent" int8 DEFAULT NULL
)
WITH (OIDS=FALSE);
ALTER TABLE "nodes" OWNER TO "postgres";

-- ----------------------------
--  Table structure for "resource_metas"
-- ----------------------------
DROP TABLE IF EXISTS "resource_metas";
CREATE TABLE "resource_metas" (
	"id" varchar NOT NULL DEFAULT NULL,
	"name" varchar NOT NULL DEFAULT NULL,
	"filepath" varchar NOT NULL DEFAULT NULL,
	"type_id" varchar NOT NULL DEFAULT NULL
)
WITH (OIDS=FALSE);
ALTER TABLE "resource_metas" OWNER TO "postgres";

-- ----------------------------
--  Table structure for "resource_types"
-- ----------------------------
DROP TABLE IF EXISTS "resource_types";
CREATE TABLE "resource_types" (
	"id" varchar NOT NULL DEFAULT NULL,
	"name" varchar NOT NULL DEFAULT NULL,
	"cover_id" varchar NOT NULL DEFAULT NULL
)
WITH (OIDS=FALSE);
ALTER TABLE "resource_types" OWNER TO "postgres";


-- ----------------------------
--  Alter sequences owned by
-- ----------------------------
ALTER SEQUENCE "accounts_id_seq" OWNED BY "accounts"."id";
-- ----------------------------
--  Primary key structure for table "accounts"
-- ----------------------------
ALTER TABLE "accounts" ADD CONSTRAINT "accounts_pkey" PRIMARY KEY ("id");

-- ----------------------------
--  Primary key structure for table "person_metas"
-- ----------------------------
ALTER TABLE "person_metas" ADD CONSTRAINT "person_metas_pkey" PRIMARY KEY ("id");

-- ----------------------------
--  Primary key structure for table "rsc_metas"
-- ----------------------------
ALTER TABLE "rsc_metas" ADD CONSTRAINT "rsc_metas_pkey" PRIMARY KEY ("id");

-- ----------------------------
--  Primary key structure for table "category_metas"
-- ----------------------------
ALTER TABLE "category_metas" ADD CONSTRAINT "category_metas_pkey" PRIMARY KEY ("id");

-- ----------------------------
--  Primary key structure for table "nodes"
-- ----------------------------
ALTER TABLE "nodes" ADD CONSTRAINT "nodes_pkey" PRIMARY KEY ("id");

