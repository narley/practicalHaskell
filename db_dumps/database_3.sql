--
-- PostgreSQL database dump
--

-- Dumped from database version 10.5
-- Dumped by pg_dump version 10.5

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: plpgsql; Type: EXTENSION; Schema: -; Owner: 
--

CREATE EXTENSION IF NOT EXISTS plpgsql WITH SCHEMA pg_catalog;


--
-- Name: EXTENSION plpgsql; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION plpgsql IS 'PL/pgSQL procedural language';


SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: article_reactions; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.article_reactions (
    id bigint NOT NULL,
    article_id bigint NOT NULL,
    user_id bigint,
    type character varying NOT NULL,
    metadata character varying NOT NULL
);


ALTER TABLE public.article_reactions OWNER TO postgres;

--
-- Name: article_reactions_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public.article_reactions_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.article_reactions_id_seq OWNER TO postgres;

--
-- Name: article_reactions_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE public.article_reactions_id_seq OWNED BY public.article_reactions.id;


--
-- Name: articles; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.articles (
    id bigint NOT NULL,
    title character varying NOT NULL,
    body character varying NOT NULL,
    published_at timestamp with time zone NOT NULL,
    author_id bigint NOT NULL
);


ALTER TABLE public.articles OWNER TO postgres;

--
-- Name: articles_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public.articles_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.articles_id_seq OWNER TO postgres;

--
-- Name: articles_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE public.articles_id_seq OWNED BY public.articles.id;


--
-- Name: auth_data; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.auth_data (
    id bigint NOT NULL,
    user_id bigint NOT NULL,
    hash_string bytea NOT NULL,
    user_type character varying NOT NULL
);


ALTER TABLE public.auth_data OWNER TO postgres;

--
-- Name: auth_data_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public.auth_data_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.auth_data_id_seq OWNER TO postgres;

--
-- Name: auth_data_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE public.auth_data_id_seq OWNED BY public.auth_data.id;


--
-- Name: comments; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.comments (
    id bigint NOT NULL,
    body character varying NOT NULL,
    user_id bigint NOT NULL,
    article_id bigint NOT NULL,
    submitted_at timestamp with time zone NOT NULL
);


ALTER TABLE public.comments OWNER TO postgres;

--
-- Name: comments_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public.comments_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.comments_id_seq OWNER TO postgres;

--
-- Name: comments_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE public.comments_id_seq OWNED BY public.comments.id;


--
-- Name: login_tokens; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.login_tokens (
    id bigint NOT NULL,
    user_id bigint NOT NULL,
    cookie character varying NOT NULL
);


ALTER TABLE public.login_tokens OWNER TO postgres;

--
-- Name: login_tokens_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public.login_tokens_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.login_tokens_id_seq OWNER TO postgres;

--
-- Name: login_tokens_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE public.login_tokens_id_seq OWNED BY public.login_tokens.id;


--
-- Name: users; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.users (
    id bigint NOT NULL,
    name character varying NOT NULL,
    email character varying NOT NULL,
    age bigint NOT NULL
);


ALTER TABLE public.users OWNER TO postgres;

--
-- Name: users_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE public.users_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.users_id_seq OWNER TO postgres;

--
-- Name: users_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE public.users_id_seq OWNED BY public.users.id;


--
-- Name: article_reactions id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.article_reactions ALTER COLUMN id SET DEFAULT nextval('public.article_reactions_id_seq'::regclass);


--
-- Name: articles id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.articles ALTER COLUMN id SET DEFAULT nextval('public.articles_id_seq'::regclass);


--
-- Name: auth_data id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.auth_data ALTER COLUMN id SET DEFAULT nextval('public.auth_data_id_seq'::regclass);


--
-- Name: comments id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.comments ALTER COLUMN id SET DEFAULT nextval('public.comments_id_seq'::regclass);


--
-- Name: login_tokens id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.login_tokens ALTER COLUMN id SET DEFAULT nextval('public.login_tokens_id_seq'::regclass);


--
-- Name: users id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.users ALTER COLUMN id SET DEFAULT nextval('public.users_id_seq'::regclass);


--
-- Data for Name: article_reactions; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.article_reactions (id, article_id, user_id, type, metadata) FROM stdin;
\.


--
-- Data for Name: articles; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.articles (id, title, body, published_at, author_id) FROM stdin;
87	You Don't Have To Be A Big Corporation To Start PROGRAMMING HASKELL	WRITING CODE Is Essential For Your Success. Read This To Find Out Why	2018-11-25 10:03:02-08	96
88	The Number One Reason You Should (Do) PROGRAMMING HASKELL	Have You Heard? WRITING CODE Is Your Best Bet To Grow	2017-05-25 08:11:04-07	60
89	Are You Making These PROGRAMMING HASKELL Mistakes?	If WRITING CODE Is So Terrible, Why Don't Statistics Show It?	2018-08-23 07:03:57-07	28
90	Here Is A Quick Cure For PROGRAMMING HASKELL	World Class Tools Make WRITING CODE Push Button Easy	2018-11-20 08:10:07-08	36
91	What Everyone Ought To Know About PROGRAMMING HASKELL	WRITING CODE Strategies For Beginners	2018-03-22 02:25:14-07	66
92	What You Should Have Asked Your Teachers About PROGRAMMING HASKELL	Don't Be Fooled By WRITING CODE	2017-11-23 05:49:18-08	95
93	How To Become Better With PROGRAMMING HASKELL In 10 Minutes	How To Make Your Product Stand Out With WRITING CODE	2017-07-29 12:27:08-07	70
94	3 Ways To Have (A) More Appealing PROGRAMMING HASKELL	Essential WRITING CODE Smartphone Apps	2017-10-14 00:52:07-07	10
95	4 Ways You Can Grow Your Creativity Using PROGRAMMING HASKELL	How To Take The Headache Out Of WRITING CODE	2017-11-04 17:32:01-07	45
96	What Everyone Must Know About PROGRAMMING HASKELL	Don't Just Sit There! Start WRITING CODE	2019-01-24 19:50:45-08	93
97	The Death Of PROGRAMMING HASKELL And How To Avoid It	Interesting Facts I Bet You Never Knew About WRITING CODE	2019-05-31 01:55:42-07	76
98	PROGRAMMING HASKELL Your Way To Success	The Best Way To WRITING CODE	2018-12-24 17:25:09-08	62
99	5 Brilliant Ways To Use PROGRAMMING HASKELL	WRITING CODE Made Simple - Even Your Kids Can Do It	2018-04-07 00:53:10-07	74
100	Boost Your PROGRAMMING HASKELL With These Tips	WRITING CODE? It's Easy If You Do It Smart	2018-05-09 10:18:49-07	18
101	12 Questions Answered About PROGRAMMING HASKELL	No More Mistakes With WRITING CODE	2019-03-01 20:27:19-08	2
102	9 Ways PROGRAMMING HASKELL Can Make You Invincible	Do WRITING CODE Better Than Barack Obama	2017-01-25 01:32:51-08	54
103	Where Is The Best PROGRAMMING HASKELL?	15 Tips For WRITING CODE Success	2018-12-04 13:51:23-08	4
104	The Ultimate Guide To PROGRAMMING HASKELL	Take The Stress Out Of WRITING CODE	2017-02-27 19:36:56-08	53
105	How To Turn PROGRAMMING HASKELL Into Success	Now You Can Have The WRITING CODE Of Your Dreams û Cheaper/Faster Than You Ever Imagined	2018-05-02 19:12:55-07	94
106	5 Ways PROGRAMMING HASKELL Will Help You Get More Business	Here Is A Method That Is Helping WRITING CODE	2019-05-03 12:14:57-07	94
107	The Truth About PROGRAMMING HASKELL In 3 Minutes	These 5 Simple WRITING CODE Tricks Will Pump Up Your Sales Almost Instantly	2018-10-20 13:03:18-07	16
108	Essential PROGRAMMING HASKELL Smartphone Apps	WRITING CODE Works Only Under These Conditions	2018-12-10 02:38:49-08	95
109	How To Find The Right PROGRAMMING HASKELL For Your Specific Product(Service).	Want A Thriving Business? Focus On WRITING CODE!	2017-06-07 06:35:57-07	12
110	How To Win Clients And Influence Markets with PROGRAMMING HASKELL	WRITING CODE Is Bound To Make An Impact In Your Business	2018-08-04 00:09:51-07	12
111	PROGRAMMING HASKELL: This Is What Professionals Do	Why Everything You Know About WRITING CODE Is A Lie	2017-07-23 05:38:45-07	36
112	Are You Embarrassed By Your PROGRAMMING HASKELL Skills? Here's What To Do	3 Ways Twitter Destroyed My WRITING CODE Without Me Noticing	2018-12-19 01:26:11-08	45
113	What Zombies Can Teach You About PROGRAMMING HASKELL	5 Simple Steps To An Effective WRITING CODE Strategy	2018-01-11 19:02:33-08	71
114	Don't Fall For This PROGRAMMING HASKELL Scam	10 Unforgivable Sins Of WRITING CODE	2019-01-20 07:03:27-08	98
115	Got Stuck? Try These Tips To Streamline Your PROGRAMMING HASKELL	Never Changing WRITING CODE Will Eventually Destroy You	2017-06-10 07:27:31-07	17
116	PROGRAMMING HASKELL An Incredibly Easy Method That Works For All	Revolutionize Your WRITING CODE With These Easy-peasy Tips	2018-01-22 14:27:05-08	32
117	Have You Heard? PROGRAMMING HASKELL Is Your Best Bet To Grow	Congratulations! Your WRITING CODE Is (Are) About To Stop Being Relevant	2018-04-06 02:35:00-07	56
118	10 Warning Signs Of Your PROGRAMMING HASKELL Demise	Where Is The Best WRITING CODE?	2018-12-01 04:16:56-08	26
119	Answered: Your Most Burning Questions About PROGRAMMING HASKELL	2 Ways You Can Use WRITING CODE To Become Irresistible To Customers	2017-12-25 01:35:39-08	2
120	5 Simple Steps To An Effective PROGRAMMING HASKELL Strategy	The Philosophy Of WRITING CODE	2018-02-25 15:21:03-08	49
121	22 Tips To Start Building A PROGRAMMING HASKELL You Always Wanted	How To Save Money with WRITING CODE?	2017-11-24 20:12:22-08	4
122	Why Most People Will Never Be Great At PROGRAMMING HASKELL	7 Rules About WRITING CODE Meant To Be Broken	2017-07-14 17:05:12-07	81
123	The Ultimate Deal On PROGRAMMING HASKELL	5 Reasons WRITING CODE Is A Waste Of Time	2018-04-16 21:51:17-07	69
124	The Secrets To Finding World Class Tools For Your PROGRAMMING HASKELL Quickly	9 Ridiculous Rules About WRITING CODE	2017-06-25 09:54:22-07	52
125	Don't Be Fooled By PROGRAMMING HASKELL	Fall In Love With WRITING CODE	2019-02-06 16:00:24-08	82
126	What Can You Do About PROGRAMMING HASKELL Right Now	The Untold Secret To Mastering WRITING CODE In Just 3 Days	2017-09-05 17:47:46-07	87
127	Double Your Profit With These 5 Tips on PROGRAMMING HASKELL	Is WRITING CODE Worth [$] To You?	2018-01-08 00:31:12-08	22
128	Ho To (Do) PROGRAMMING HASKELL Without Leaving Your Office(House).	The Secrets To WRITING CODE	2017-11-28 03:05:03-08	10
129	Top 10 Tips To Grow Your PROGRAMMING HASKELL	3 Easy Ways To Make WRITING CODE Faster	2017-07-04 01:40:36-07	9
130	5 Sexy Ways To Improve Your PROGRAMMING HASKELL	The WRITING CODE That Wins Customers	2019-01-29 14:57:18-08	46
131	Clear And Unbiased Facts About PROGRAMMING HASKELL (Without All the Hype)	5 Incredibly Useful WRITING CODE Tips For Small Businesses	2017-02-23 03:39:24-08	68
132	10 Best Practices For PROGRAMMING HASKELL	Being A Star In Your Industry Is A Matter Of WRITING CODE	2018-10-01 03:50:44-07	85
133	Find A Quick Way To PROGRAMMING HASKELL	Should Fixing WRITING CODE Take 60 Steps?	2017-09-14 09:08:06-07	92
134	Being A Star In Your Industry Is A Matter Of PROGRAMMING HASKELL	Who Else Wants To Know The Mystery Behind WRITING CODE?	2017-05-03 15:06:21-07	33
135	How To Earn $398/Day Using PROGRAMMING HASKELL	WRITING CODE And Love Have 4 Things In Common	2018-03-15 10:28:38-07	68
136	PROGRAMMING HASKELL Works Only Under These Conditions	In 10 Minutes, I'll Give You The Truth About WRITING CODE	2018-07-01 04:30:56-07	77
137	17 Tricks About PROGRAMMING HASKELL You Wish You Knew Before	10 Tips That Will Make You Influential In WRITING CODE	2017-03-26 05:28:14-07	26
138	15 Tips For PROGRAMMING HASKELL Success	The Hidden Mystery Behind WRITING CODE	2019-04-11 20:53:26-07	17
139	Can You Really Find PROGRAMMING HASKELL (on the Web)?	Fear? Not If You Use WRITING CODE The Right Way!	2018-11-29 14:44:16-08	45
140	How To Get (A) Fabulous PROGRAMMING HASKELL On A Tight Budget	You Don't Have To Be A Big Corporation To Start WRITING CODE	2017-04-04 12:15:18-07	58
141	How To Win Friends And Influence People with PROGRAMMING HASKELL	How WRITING CODE Made Me A Better Salesperson	2019-01-17 00:28:05-08	28
142	Who Else Wants To Know The Mystery Behind PROGRAMMING HASKELL?	Winning Tactics For WRITING CODE	2017-04-15 14:23:51-07	22
143	How To Start A Business With PROGRAMMING HASKELL	3 Tips About WRITING CODE You Can't Afford To Miss	2019-03-03 20:34:44-08	33
144	11 Methods Of PROGRAMMING HASKELL Domination	5 Ways Of WRITING CODE That Can Drive You Bankrupt - Fast!	2019-04-07 15:50:32-07	89
145	3 Ways You Can Reinvent PROGRAMMING HASKELL Without Looking Like An Amateur	ThereÆs Big Money In WRITING CODE	2017-07-31 17:47:16-07	42
146	10 Tips That Will Make You Influential In PROGRAMMING HASKELL	The Number One Reason You Should (Do) WRITING CODE	2017-12-24 10:48:11-08	75
147	Don't Just Sit There! Start PROGRAMMING HASKELL	It's All About (The) WRITING CODE	2017-01-19 04:33:13-08	7
148	Everything You Wanted to Know About PROGRAMMING HASKELL and Were Afraid To Ask	3 Ways To Have (A) More Appealing WRITING CODE	2018-06-05 06:48:49-07	17
149	Fall In Love With PROGRAMMING HASKELL	WRITING CODE: An Incredibly Easy Method That Works For All	2017-05-30 09:02:19-07	31
150	Death, PROGRAMMING HASKELL And Taxes	Here Is A Quick Cure For WRITING CODE	2018-08-31 01:16:39-07	81
151	Why I Hate PROGRAMMING HASKELL	3 WRITING CODE Secrets You Never Knew	2019-02-16 11:23:34-08	98
152	Listen To Your Customers. They Will Tell You All About PROGRAMMING HASKELL	What Can Instagramm Teach You About WRITING CODE	2019-03-28 04:12:30-07	98
153	Fascinating PROGRAMMING HASKELL Tactics That Can Help Your Business Grow	To People That Want To Start WRITING CODE But Are Affraid To Get Started	2017-07-16 14:12:41-07	6
154	10 Unforgivable Sins Of PROGRAMMING HASKELL	Where Can You Find Free WRITING CODE Resources	2018-04-08 05:43:31-07	26
155	It's All About (The) PROGRAMMING HASKELL	Can You Really Find WRITING CODE (on the Web)?	2018-09-23 05:31:21-07	73
156	Avoid The Top 10 PROGRAMMING HASKELL Mistakes	5 Secrets: How To Use WRITING CODE To Create A Successful Business(Product)	2017-10-07 03:16:45-07	48
157	5 Best Ways To Sell PROGRAMMING HASKELL	WRITING CODE An Incredibly Easy Method That Works For All	2018-10-25 13:21:28-07	68
158	Little Known Ways to PROGRAMMING HASKELL	5 Sexy Ways To Improve Your WRITING CODE	2018-05-30 07:47:40-07	24
159	2 Ways You Can Use PROGRAMMING HASKELL To Become Irresistible To Customers	Believe In Your WRITING CODE Skills But Never Stop Improving	2018-04-18 07:08:32-07	10
160	The Secret of Successful PROGRAMMING HASKELL	Get The Most Out of WRITING CODE and Facebook	2017-10-17 12:19:33-07	15
161	I Don't Want To Spend This Much Time On PROGRAMMING HASKELL. How About You?	14 Days To A Better WRITING CODE	2017-03-06 02:06:04-08	99
162	Stop Wasting Time And Start PROGRAMMING HASKELL	Secrets To Getting WRITING CODE To Complete Tasks Quickly And Efficiently	2018-10-16 12:36:51-07	97
163	5 Ways Of PROGRAMMING HASKELL That Can Drive You Bankrupt - Fast!	The Quickest & Easiest Way To WRITING CODE	2018-09-01 15:27:42-07	2
164	Marriage And PROGRAMMING HASKELL Have More In Common Than You Think	The Death Of WRITING CODE And How To Avoid It	2018-10-26 01:40:27-07	91
165	PROGRAMMING HASKELL And Love - How They Are The Same	Take 10 Minutes to Get Started With WRITING CODE	2019-01-30 16:42:57-08	76
166	OMG! The Best PROGRAMMING HASKELL Ever!	5 Easy Ways You Can Turn WRITING CODE Into Success	2017-07-20 06:01:08-07	10
167	Find Out How I Cured My PROGRAMMING HASKELL In 2 Days	WRITING CODE: The Samurai Way	2019-01-14 08:02:36-08	29
168	Remarkable Website - PROGRAMMING HASKELL Will Help You Get There	What Is WRITING CODE and How Does It Work?	2019-04-20 06:59:02-07	32
169	Is PROGRAMMING HASKELL Worth [$] To You?	How To Lose Money With WRITING CODE	2017-04-23 16:45:44-07	26
170	You Can Thank Us Later - 3 Reasons To Stop Thinking About PROGRAMMING HASKELL	How To Learn WRITING CODE	2018-01-18 11:23:59-08	90
171	How To Use PROGRAMMING HASKELL To Desire	How To Start A Business With WRITING CODE	2019-04-10 23:19:26-07	46
172	Learn To (Do) PROGRAMMING HASKELL Like A Professional	5 Things To Do Immediately About WRITING CODE	2017-06-21 15:10:46-07	60
173	PROGRAMMING HASKELL Smackdown!	3 Ways To Master WRITING CODE Without Breaking A Sweat	2019-03-10 18:47:02-07	14
174	Wondering How To Make Your PROGRAMMING HASKELL Rock? Read This!	3 Things Everyone Knows About WRITING CODE That You Don't	2017-05-19 10:30:51-07	77
175	5 Ways You Can Get More PROGRAMMING HASKELL While Spending Less	This Study Will Perfect Your WRITING CODE: Read Or Miss Out	2017-04-16 03:00:37-07	61
176	The Ugly Truth About PROGRAMMING HASKELL	The Truth Is You Are Not The Only Person Concerned About WRITING CODE	2019-03-02 13:57:20-08	24
177	The Single Most Important Thing You Need To Know About PROGRAMMING HASKELL	Proof That WRITING CODE Really Works	2017-06-11 09:31:18-07	16
178	A Guide To PROGRAMMING HASKELL At Any Age	Learn Exactly How We Made WRITING CODE Last Month	2018-10-29 23:26:17-07	41
179	Sexy PROGRAMMING HASKELL	WRITING CODE Iphone Apps	2018-09-08 05:15:45-07	6
180	Top 3 Ways To Buy A Used PROGRAMMING HASKELL	WRITING CODE Smackdown!	2017-07-16 23:55:30-07	7
181	World Class Tools Make PROGRAMMING HASKELL Push Button Easy	How To Teach WRITING CODE Better Than Anyone Else	2017-04-02 10:40:06-07	32
182	Top 10 Tips With PROGRAMMING HASKELL	What Alberto Savoia Can Teach You About WRITING CODE	2019-01-27 08:24:39-08	59
183	The Ultimate Secret Of PROGRAMMING HASKELL	Marriage And WRITING CODE Have More In Common Than You Think	2017-09-30 11:44:13-07	88
184	Now You Can Have Your PROGRAMMING HASKELL Done Safely	How To Turn WRITING CODE Into Success	2018-06-03 02:02:24-07	46
185	Apply These 5 Secret Techniques To Improve PROGRAMMING HASKELL	5 Problems Everyone Has With WRITING CODE û How To Solved Them	2018-10-04 05:08:13-07	19
186	The Secret of PROGRAMMING HASKELL	Apply These 5 Secret Techniques To Improve WRITING CODE	2018-04-12 22:23:05-07	58
\.


--
-- Data for Name: auth_data; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.auth_data (id, user_id, hash_string, user_type) FROM stdin;
1	104	\\x7368613235367c31377c5834674b61377637755761616146464f7a69572f56413d3d7c70707576644451734c68592f43676c6955383866452b395044546c35533151715878647867323757736d383d	admin
2	50	\\x7368613235367c31377c744e663538454b36456e684b31516c704630717230413d3d7c5742716f726f6e5a71755636624355367a30557467307162416a336141506331383071695333564b347a4d3d	user
3	80	\\x7368613235367c31377c4533783155624f5746613143466850353870704938513d3d7c554632667874736c4f524130624545796349664f763454415563694c2f334a3947594239776c5865416e633d	user
\.


--
-- Data for Name: comments; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.comments (id, body, user_id, article_id, submitted_at) FROM stdin;
1	Your site has some novel content.	61	97	2019-06-01 13:21:24-07
2	This has some interesting points of view.	3	156	2017-10-08 08:05:18-07
3	I'd never thought about this subject like that before.	22	176	2019-03-07 23:42:59-08
4	I have a few qualms with this.	9	89	2018-08-26 21:08:43-07
5	This has some interesting points of view.	41	112	2018-12-21 08:00:19-08
6	Thanks for writing this!	55	87	2018-11-28 10:44:23-08
7	This is outstanding!	43	121	2017-11-25 09:37:14-08
8	The overall grammar and structure could be improved.	7	118	2018-12-05 18:30:24-08
9	This has some interesting points of view.	87	102	2017-01-28 04:05:01-08
10	I liked some of this. Other parts not so much.	2	145	2017-08-04 21:16:23-07
11	This article was really great!	6	185	2018-10-10 23:56:09-07
12	The overall grammar and structure could be improved.	36	91	2018-03-26 01:57:16-07
13	This is outstanding!	100	153	2017-07-22 15:00:17-07
14	Thanks for writing this!	18	143	2019-03-03 20:57:16-08
15	Your site has some novel content.	23	169	2017-04-25 03:02:52-07
16	I have a few qualms with this.	15	182	2019-01-29 02:11:13-08
17	I'd never thought about this subject like that before.	101	110	2018-08-04 04:38:17-07
18	I'm not sure I agree with your point about this.	54	126	2017-09-06 19:17:31-07
19	This article was really great!	54	160	2017-10-20 13:28:10-07
20	I liked some of this. Other parts not so much.	94	156	2017-10-13 00:44:47-07
21	I liked some of this. Other parts not so much.	24	147	2017-01-19 06:40:43-08
22	Great work!	13	97	2019-06-02 16:33:54-07
23	I really liked this article!	86	177	2017-06-17 23:52:38-07
24	You're certainly unique in your perspective on this.	83	143	2019-03-10 14:55:47-07
25	There are a lot of ways to improve your writing.	79	98	2018-12-27 10:06:52-08
26	I'd never thought about this subject like that before.	64	135	2018-03-16 02:45:23-07
27	Great work!	28	120	2018-02-28 17:54:19-08
28	The overall grammar and structure could be improved.	84	152	2019-04-02 13:12:33-07
29	Your site has some novel content.	69	104	2017-03-04 02:51:00-08
30	This article was really great!	98	142	2017-04-18 00:32:59-07
31	Your site has some novel content.	32	116	2018-01-28 08:49:41-08
32	You're certainly unique in your perspective on this.	51	178	2018-11-01 05:20:51-07
33	Thanks for writing this!	48	126	2017-09-12 00:38:51-07
34	You're certainly unique in your perspective on this.	61	131	2017-03-01 07:10:05-08
35	Fascinating insights.	46	110	2018-08-09 08:02:50-07
36	I have a few qualms with this.	77	175	2017-04-17 12:39:41-07
37	The overall grammar and structure could be improved.	41	149	2017-06-01 10:51:37-07
38	There are a lot of ways to improve your writing.	89	90	2018-11-22 07:51:46-08
39	I really liked this article!	76	170	2018-01-20 21:57:41-08
40	Thanks for writing this!	20	149	2017-06-01 11:49:37-07
41	Thanks for writing this!	76	172	2017-06-25 05:46:00-07
42	This article was really great!	10	113	2018-01-15 06:47:05-08
43	Thanks for writing this!	6	180	2017-07-19 07:05:52-07
44	Fascinating insights.	91	110	2018-08-06 12:53:14-07
45	Your site has some novel content.	35	164	2018-11-01 13:30:44-07
46	I really liked this article!	48	146	2017-12-28 20:23:38-08
47	I liked some of this. Other parts not so much.	45	154	2018-04-15 03:45:22-07
48	Fascinating insights.	33	109	2017-06-10 15:24:45-07
49	I'm not sure I agree with your point about this.	55	152	2019-03-29 01:53:56-07
50	Your work is an inspiration!	56	87	2018-12-01 14:16:19-08
51	I liked some of this. Other parts not so much.	13	97	2019-06-01 03:18:20-07
52	There are a lot of ways to improve your writing.	59	170	2018-01-20 03:19:34-08
53	This article was really great!	80	142	2017-04-20 21:53:14-07
54	This has some interesting points of view.	57	165	2019-02-01 14:07:42-08
55	Thanks for writing this!	6	166	2017-07-21 14:36:24-07
56	I'd never thought about this subject like that before.	69	155	2018-09-28 14:04:37-07
57	Great work!	36	157	2018-10-31 21:17:50-07
58	Your work is an inspiration!	76	117	2018-04-11 20:16:04-07
59	I have a few qualms with this.	4	104	2017-03-06 02:48:09-08
60	Great work!	33	147	2017-01-20 19:38:38-08
61	Your site has some novel content.	53	127	2018-01-09 03:20:13-08
62	There are a lot of ways to improve your writing.	97	119	2017-12-31 03:07:12-08
63	You're certainly unique in your perspective on this.	75	102	2017-01-29 11:52:00-08
64	I have a few qualms with this.	10	125	2019-02-11 03:34:50-08
65	Great work!	80	104	2017-03-05 01:44:05-08
66	I'd like to see this topic explored in a little more depth.	19	129	2017-07-07 17:23:03-07
67	This is outstanding!	72	94	2017-10-15 07:52:22-07
68	I'm not sure I agree with your point about this.	79	91	2018-03-23 01:31:48-07
69	I'm not sure I agree with your point about this.	46	156	2017-10-09 17:58:33-07
70	This article was really great!	10	128	2017-11-28 11:09:12-08
71	This has some interesting points of view.	47	88	2017-06-01 04:12:26-07
72	Fascinating insights.	92	115	2017-06-14 01:35:42-07
73	The overall grammar and structure could be improved.	70	181	2017-04-06 04:35:39-07
74	The overall grammar and structure could be improved.	37	131	2017-02-23 19:01:12-08
75	Your site has some novel content.	42	130	2019-02-02 05:31:40-08
76	I liked some of this. Other parts not so much.	67	186	2018-04-19 14:49:40-07
77	Your site has some novel content.	99	152	2019-04-03 16:47:11-07
78	I'd never thought about this subject like that before.	66	117	2018-04-10 01:30:11-07
79	Great work!	20	95	2017-11-05 09:05:13-08
80	Your site has some novel content.	45	176	2019-03-03 10:06:20-08
81	Your work is an inspiration!	100	132	2018-10-06 10:39:04-07
82	I really liked this article!	59	136	2018-07-02 09:09:26-07
83	Great work!	94	117	2018-04-06 15:32:20-07
84	There are a lot of ways to improve your writing.	60	146	2017-12-24 16:11:14-08
85	I'd never thought about this subject like that before.	9	88	2017-05-29 17:16:51-07
86	I liked some of this. Other parts not so much.	86	87	2018-12-01 16:40:52-08
87	I'm not sure I agree with your point about this.	81	154	2018-04-12 13:18:03-07
88	You're certainly unique in your perspective on this.	5	158	2018-06-02 20:02:14-07
89	Your work is an inspiration!	68	112	2018-12-19 17:24:06-08
90	This is outstanding!	83	145	2017-08-06 23:30:47-07
91	I'm not sure I agree with your point about this.	44	178	2018-11-03 04:25:36-07
92	There are a lot of ways to improve your writing.	36	122	2017-07-15 13:09:34-07
93	Your work is an inspiration!	6	137	2017-03-29 15:32:03-07
94	You're certainly unique in your perspective on this.	48	114	2019-01-26 08:14:43-08
95	I'm not sure I agree with your point about this.	6	172	2017-06-27 16:56:11-07
96	Your site has some novel content.	34	164	2018-11-02 01:01:42-07
97	Thanks for writing this!	95	101	2019-03-05 08:47:59-08
98	Fascinating insights.	61	113	2018-01-15 14:51:35-08
99	Fascinating insights.	42	168	2019-04-21 00:49:07-07
100	I'd never thought about this subject like that before.	89	148	2018-06-06 23:18:58-07
101	I'm not sure I agree with your point about this.	94	137	2017-03-31 15:34:24-07
102	This is outstanding!	51	131	2017-03-01 13:13:11-08
103	Your work is an inspiration!	32	167	2019-01-14 09:35:25-08
104	You're certainly unique in your perspective on this.	87	105	2018-05-08 16:12:07-07
105	I'm not sure I agree with your point about this.	5	147	2017-01-23 14:32:45-08
106	There are a lot of ways to improve your writing.	19	111	2017-07-24 13:26:02-07
107	I'd like to see this topic explored in a little more depth.	30	166	2017-07-24 10:12:01-07
108	Your site has some novel content.	76	111	2017-07-25 16:44:40-07
109	Did you consider the alternative argument posted today?	27	167	2019-01-15 10:54:52-08
110	Thanks for writing this!	89	184	2018-06-08 10:52:41-07
111	You're certainly unique in your perspective on this.	55	129	2017-07-08 23:37:15-07
112	Your work is an inspiration!	96	110	2018-08-04 02:32:32-07
113	This is outstanding!	12	90	2018-11-21 07:22:30-08
114	I'm not sure I agree with your point about this.	9	172	2017-06-22 10:01:14-07
115	This is outstanding!	28	125	2019-02-08 00:25:04-08
116	The overall grammar and structure could be improved.	17	166	2017-07-20 09:46:25-07
117	Fascinating insights.	36	134	2017-05-04 23:10:50-07
118	I liked some of this. Other parts not so much.	58	180	2017-07-21 03:40:31-07
119	The overall grammar and structure could be improved.	75	116	2018-01-25 16:07:52-08
120	The overall grammar and structure could be improved.	77	165	2019-02-01 05:06:12-08
121	You're certainly unique in your perspective on this.	45	167	2019-01-19 20:28:58-08
122	Your site has some novel content.	69	104	2017-03-05 00:36:54-08
123	This article was really great!	98	99	2018-04-10 21:13:20-07
124	This has some interesting points of view.	77	109	2017-06-13 18:38:29-07
125	Thanks for writing this!	60	178	2018-10-30 11:13:57-07
126	This article was really great!	80	98	2018-12-25 00:02:39-08
127	You're certainly unique in your perspective on this.	84	142	2017-04-20 02:53:51-07
128	Your site has some novel content.	30	127	2018-01-08 22:37:32-08
129	Did you consider the alternative argument posted today?	51	120	2018-03-03 13:11:01-08
130	I liked some of this. Other parts not so much.	82	92	2017-11-29 22:08:53-08
131	Fascinating insights.	5	175	2017-04-20 05:22:15-07
132	The overall grammar and structure could be improved.	35	91	2018-03-23 18:45:32-07
133	The overall grammar and structure could be improved.	72	149	2017-06-05 15:12:43-07
134	This is outstanding!	29	177	2017-06-15 03:30:38-07
135	The overall grammar and structure could be improved.	52	107	2018-10-20 23:51:05-07
136	You're certainly unique in your perspective on this.	75	87	2018-11-26 15:03:31-08
137	This is outstanding!	19	118	2018-12-03 22:24:42-08
138	I'm not sure I agree with your point about this.	14	172	2017-06-24 19:13:51-07
139	There are a lot of ways to improve your writing.	79	116	2018-01-26 17:23:32-08
140	Your site has some novel content.	82	108	2018-12-10 11:07:35-08
141	Thanks for writing this!	16	160	2017-10-20 22:47:49-07
142	The overall grammar and structure could be improved.	15	117	2018-04-12 07:48:15-07
143	This has some interesting points of view.	96	160	2017-10-18 05:40:54-07
144	I'd never thought about this subject like that before.	17	115	2017-06-15 17:34:08-07
145	Great work!	23	140	2017-04-06 04:59:49-07
146	Did you consider the alternative argument posted today?	8	103	2018-12-07 09:14:06-08
147	The overall grammar and structure could be improved.	2	154	2018-04-09 17:29:21-07
148	I'm not sure I agree with your point about this.	38	88	2017-05-30 09:32:32-07
149	The overall grammar and structure could be improved.	43	178	2018-11-01 14:59:05-07
150	This article was really great!	74	151	2019-02-18 12:01:04-08
151	Thanks for writing this!	79	131	2017-02-23 17:49:37-08
152	Did you consider the alternative argument posted today?	80	132	2018-10-04 14:01:40-07
153	Great work!	95	184	2018-06-04 14:44:46-07
154	Thanks for writing this!	60	144	2019-04-08 22:58:43-07
155	I liked some of this. Other parts not so much.	51	157	2018-10-29 02:59:37-07
156	This is outstanding!	59	113	2018-01-16 13:55:00-08
157	I really liked this article!	64	113	2018-01-14 22:15:55-08
158	You're certainly unique in your perspective on this.	39	158	2018-06-02 05:28:04-07
159	Thanks for writing this!	31	116	2018-01-23 05:29:07-08
160	I liked some of this. Other parts not so much.	36	184	2018-06-07 17:55:54-07
161	I'd never thought about this subject like that before.	36	114	2019-01-26 23:01:07-08
162	I really liked this article!	30	100	2018-05-14 01:09:12-07
163	This has some interesting points of view.	13	96	2019-01-27 04:28:45-08
164	Your site has some novel content.	25	133	2017-09-14 19:44:22-07
165	I'm not sure I agree with your point about this.	66	109	2017-06-07 20:36:26-07
166	Fascinating insights.	37	135	2018-03-17 00:27:30-07
167	Your site has some novel content.	95	109	2017-06-10 20:54:33-07
168	This has some interesting points of view.	7	114	2019-01-25 18:18:44-08
169	You're certainly unique in your perspective on this.	47	180	2017-07-21 14:37:20-07
170	I'd never thought about this subject like that before.	23	104	2017-03-05 06:39:20-08
171	I liked some of this. Other parts not so much.	54	158	2018-05-30 11:40:47-07
172	Did you consider the alternative argument posted today?	58	110	2018-08-09 10:18:02-07
173	Great work!	94	127	2018-01-14 16:33:27-08
174	There are a lot of ways to improve your writing.	79	100	2018-05-12 02:41:56-07
175	I'd like to see this topic explored in a little more depth.	68	145	2017-08-06 05:37:58-07
176	I'd never thought about this subject like that before.	30	169	2017-04-27 02:20:23-07
177	I really liked this article!	4	160	2017-10-17 22:22:53-07
178	This has some interesting points of view.	97	134	2017-05-04 06:17:02-07
179	This article was really great!	37	120	2018-02-28 20:00:04-08
180	Did you consider the alternative argument posted today?	90	173	2019-03-12 21:31:16-07
181	I'd never thought about this subject like that before.	85	149	2017-06-06 05:08:55-07
182	I liked some of this. Other parts not so much.	9	140	2017-04-11 07:30:18-07
183	The overall grammar and structure could be improved.	27	174	2017-05-25 06:54:06-07
184	I'm not sure I agree with your point about this.	7	133	2017-09-20 05:57:40-07
185	I'm not sure I agree with your point about this.	41	179	2018-09-12 00:52:51-07
186	This article was really great!	21	147	2017-01-19 04:49:45-08
187	Did you consider the alternative argument posted today?	31	136	2018-07-04 01:08:41-07
188	This has some interesting points of view.	97	102	2017-01-28 14:33:07-08
189	Your site has some novel content.	12	101	2019-03-02 19:30:56-08
190	Thanks for writing this!	42	155	2018-09-28 10:24:12-07
191	Did you consider the alternative argument posted today?	23	151	2019-02-23 08:11:34-08
192	I liked some of this. Other parts not so much.	47	107	2018-10-27 10:02:44-07
193	The overall grammar and structure could be improved.	24	151	2019-02-21 13:51:58-08
194	The overall grammar and structure could be improved.	52	144	2019-04-11 20:35:14-07
195	This is outstanding!	36	88	2017-05-30 02:29:52-07
196	Fascinating insights.	58	183	2017-10-02 04:04:28-07
197	I have a few qualms with this.	93	97	2019-06-03 15:59:28-07
198	I'd like to see this topic explored in a little more depth.	93	163	2018-09-02 23:50:08-07
199	Thanks for writing this!	33	146	2017-12-25 04:42:47-08
200	You're certainly unique in your perspective on this.	86	87	2018-11-25 16:15:39-08
\.


--
-- Data for Name: login_tokens; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.login_tokens (id, user_id, cookie) FROM stdin;
\.


--
-- Data for Name: users; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.users (id, name, email, age) FROM stdin;
2	Sherri Deberry	Sherri.Deberry@test.com	40
3	Lakita Coverdale	Lakita.Coverdale@gmail.com	33
4	Justin Denby	Justin.Denby@test.com	28
5	Mikaela Harrigan	Mikaela.Harrigan@gmail.com	42
6	Venus Houlihan	Venus@Houlihan.com	43
7	Tierra Servin	Tierra.Servin@test.com	49
8	Jannie Fultz	Jannie@Fultz.com	34
9	Alisha Parisi	Alisha@Parisi.com	35
10	Denisha Hokanson	Denisha.Hokanson@test.com	40
11	Kathline Grange	Kathline.Grange@gmail.com	19
12	Suzanne Parikh	Suzanne@Parikh.com	45
13	Archie Swallow	Archie.Swallow@gmail.com	51
14	Shonta Ewert	Shonta.Ewert@test.com	32
15	Drew Burson	Drew@Burson.com	19
16	Vernetta Reed	Vernetta.Reed@test.com	26
17	Sabra Bossi	Sabra@Bossi.com	27
18	Lia Spagnuolo	Lia.Spagnuolo@test.com	53
19	Lashawn Schrimsher	Lashawn.Schrimsher@gmail.com	48
20	Wilhemina Semmes	Wilhemina.Semmes@gmail.com	27
21	Myron Corlett	Myron@Corlett.com	41
22	Tracy Infantino	Tracy.Infantino@gmail.com	45
23	Philip Laverriere	Philip@Laverriere.com	21
24	Ouida Berlin	Ouida@Berlin.com	26
25	Charity Kling	Charity@Kling.com	55
26	Larita Matley	Larita.Matley@gmail.com	32
27	Shirleen Mcgahey	Shirleen@Mcgahey.com	55
28	Illa Echavarria	Illa@Echavarria.com	31
29	Agueda Capello	Agueda@Capello.com	33
30	Jerrell Moultry	Jerrell@Moultry.com	39
31	Kandy Dragoo	Kandy@Dragoo.com	18
32	Bonita Perera	Bonita@Perera.com	48
33	Breanna Hisey	Breanna@Hisey.com	50
34	Renato Kimpel	Renato.Kimpel@test.com	20
35	Tiffanie Delgado	Tiffanie@Delgado.com	19
36	Ludie Gammill	Ludie.Gammill@test.com	42
37	Claudio Awad	Claudio@Awad.com	35
38	Alejandro Buckles	Alejandro.Buckles@gmail.com	54
39	Deana Denbow	Deana.Denbow@test.com	36
40	Cherryl Dahmer	Cherryl.Dahmer@gmail.com	33
41	Edda Richard	Edda.Richard@test.com	20
42	Leigha Risch	Leigha@Risch.com	22
43	Ruthie Christensen	Ruthie.Christensen@gmail.com	37
44	Corrina Baskins	Corrina@Baskins.com	53
45	Nettie Phoenix	Nettie.Phoenix@gmail.com	28
46	Stephnie Whitacre	Stephnie.Whitacre@gmail.com	20
47	Lou Rusek	Lou.Rusek@gmail.com	24
48	Elvis Salcedo	Elvis@Salcedo.com	33
49	Jeanie Guice	Jeanie.Guice@gmail.com	21
51	Barton Bussiere	Barton.Bussiere@gmail.com	30
52	Chelsea Angulo	Chelsea.Angulo@test.com	22
53	Yee Plum	Yee.Plum@test.com	49
54	Alpha Ferdinand	Alpha@Ferdinand.com	36
55	Eloisa Cacciatore	Eloisa.Cacciatore@gmail.com	39
56	Sharyn Mackay	Sharyn.Mackay@gmail.com	38
57	Valentine Ascencio	Valentine.Ascencio@gmail.com	26
58	Mei Fiecke	Mei.Fiecke@gmail.com	31
59	Tim Hisle	Tim.Hisle@gmail.com	23
60	Zada Brooks	Zada.Brooks@test.com	43
61	Lester Bona	Lester@Bona.com	37
62	Ludie Lehrer	Ludie.Lehrer@test.com	54
63	Mercedez Gonsales	Mercedez.Gonsales@gmail.com	22
64	Lorri Hasse	Lorri.Hasse@gmail.com	54
65	Ciara Beegle	Ciara.Beegle@test.com	43
66	Ela Sitsler	Ela.Sitsler@gmail.com	41
67	Micha Knop	Micha@Knop.com	21
68	Debi Stout	Debi.Stout@gmail.com	24
69	Ramonita Spight	Ramonita.Spight@gmail.com	43
70	Ida Escamilla	Ida.Escamilla@test.com	44
71	Flossie Whitesides	Flossie.Whitesides@gmail.com	34
72	Erik Thrash	Erik@Thrash.com	48
73	Cinderella Norfleet	Cinderella@Norfleet.com	32
74	Huey Longway	Huey.Longway@gmail.com	41
75	Jackson Broe	Jackson@Broe.com	26
76	Ai Maxwell	Ai.Maxwell@test.com	52
77	Trina Winsor	Trina.Winsor@test.com	23
78	Brittaney Kuester	Brittaney.Kuester@test.com	25
79	Lavada Fife	Lavada.Fife@test.com	19
81	Gary Earnest	Gary.Earnest@gmail.com	40
82	Alana Sylvestre	Alana@Sylvestre.com	55
83	Orpha Walters	Orpha@Walters.com	36
84	Juliet Stanton	Juliet.Stanton@gmail.com	23
85	Moises Kagan	Moises@Kagan.com	37
86	Evie Berryman	Evie@Berryman.com	30
87	Britni Hagan	Britni.Hagan@test.com	18
88	Lanita Mullins	Lanita@Mullins.com	27
89	Christin Combes	Christin@Combes.com	38
90	Angelique Wireman	Angelique.Wireman@gmail.com	35
91	Kourtney Chairez	Kourtney.Chairez@gmail.com	39
92	Saturnina Herriman	Saturnina@Herriman.com	28
93	Malena Ringer	Malena@Ringer.com	33
94	Reyna Epperly	Reyna.Epperly@test.com	36
95	Etsuko Adamek	Etsuko.Adamek@test.com	53
96	Imelda Privett	Imelda.Privett@gmail.com	51
97	Jim Rapoza	Jim@Rapoza.com	23
98	Samella Dostal	Samella.Dostal@gmail.com	32
99	Simon Athens	Simon.Athens@test.com	27
80	Romaine Trent	Romaine@Trent.com	46
100	Marlena Watt	Marlena@Watt.com	53
101	Glayds Blackford	Glayds.Blackford@test.com	51
104	Admin	admin@test.com	30
50	Santa Washington	Santa@Washington.com	55
\.


--
-- Name: article_reactions_id_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('public.article_reactions_id_seq', 1, false);


--
-- Name: articles_id_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('public.articles_id_seq', 186, true);


--
-- Name: auth_data_id_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('public.auth_data_id_seq', 3, true);


--
-- Name: comments_id_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('public.comments_id_seq', 200, true);


--
-- Name: login_tokens_id_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('public.login_tokens_id_seq', 10, true);


--
-- Name: users_id_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('public.users_id_seq', 106, true);


--
-- Name: article_reactions article_reactions_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.article_reactions
    ADD CONSTRAINT article_reactions_pkey PRIMARY KEY (id);


--
-- Name: articles articles_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.articles
    ADD CONSTRAINT articles_pkey PRIMARY KEY (id);


--
-- Name: auth_data auth_data_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.auth_data
    ADD CONSTRAINT auth_data_pkey PRIMARY KEY (id);


--
-- Name: comments comments_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.comments
    ADD CONSTRAINT comments_pkey PRIMARY KEY (id);


--
-- Name: login_tokens login_tokens_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.login_tokens
    ADD CONSTRAINT login_tokens_pkey PRIMARY KEY (id);


--
-- Name: users unique_email; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.users
    ADD CONSTRAINT unique_email UNIQUE (email);


--
-- Name: auth_data unique_user_id; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.auth_data
    ADD CONSTRAINT unique_user_id UNIQUE (user_id);


--
-- Name: login_tokens unique_userid; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.login_tokens
    ADD CONSTRAINT unique_userid UNIQUE (user_id);


--
-- Name: users users_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.users
    ADD CONSTRAINT users_pkey PRIMARY KEY (id);


--
-- Name: article_reactions article_reactions_article_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.article_reactions
    ADD CONSTRAINT article_reactions_article_id_fkey FOREIGN KEY (article_id) REFERENCES public.articles(id);


--
-- Name: article_reactions article_reactions_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.article_reactions
    ADD CONSTRAINT article_reactions_user_id_fkey FOREIGN KEY (user_id) REFERENCES public.users(id);


--
-- Name: articles articles_author_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.articles
    ADD CONSTRAINT articles_author_id_fkey FOREIGN KEY (author_id) REFERENCES public.users(id);


--
-- Name: auth_data auth_data_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.auth_data
    ADD CONSTRAINT auth_data_user_id_fkey FOREIGN KEY (user_id) REFERENCES public.users(id);


--
-- Name: comments comments_article_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.comments
    ADD CONSTRAINT comments_article_id_fkey FOREIGN KEY (article_id) REFERENCES public.articles(id);


--
-- Name: comments comments_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.comments
    ADD CONSTRAINT comments_user_id_fkey FOREIGN KEY (user_id) REFERENCES public.users(id);


--
-- Name: login_tokens login_tokens_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.login_tokens
    ADD CONSTRAINT login_tokens_user_id_fkey FOREIGN KEY (user_id) REFERENCES public.users(id);


--
-- PostgreSQL database dump complete
--

