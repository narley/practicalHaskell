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
-- Name: articles; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.articles (
    id bigint NOT NULL,
    title character varying NOT NULL,
    body character varying NOT NULL,
    published_at timestamp with time zone NOT NULL
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
-- Name: articles id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.articles ALTER COLUMN id SET DEFAULT nextval('public.articles_id_seq'::regclass);


--
-- Name: users id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.users ALTER COLUMN id SET DEFAULT nextval('public.users_id_seq'::regclass);


--
-- Data for Name: articles; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.articles (id, title, body, published_at) FROM stdin;
2	You Don't Have To Be A Big Corporation To Start PROGRAMMING HASKELL	WRITING CODE Is Essential For Your Success. Read This To Find Out Why	2018-09-10 18:03:57-07
3	The Number One Reason You Should (Do) PROGRAMMING HASKELL	Have You Heard? WRITING CODE Is Your Best Bet To Grow	2017-09-29 00:13:57-07
4	Are You Making These PROGRAMMING HASKELL Mistakes?	If WRITING CODE Is So Terrible, Why Don't Statistics Show It?	2019-04-17 03:51:32-07
5	Here Is A Quick Cure For PROGRAMMING HASKELL	World Class Tools Make WRITING CODE Push Button Easy	2017-10-02 10:23:18-07
6	What Everyone Ought To Know About PROGRAMMING HASKELL	WRITING CODE Strategies For Beginners	2019-04-21 17:41:38-07
7	What You Should Have Asked Your Teachers About PROGRAMMING HASKELL	Don't Be Fooled By WRITING CODE	2018-02-24 04:39:45-08
8	How To Become Better With PROGRAMMING HASKELL In 10 Minutes	How To Make Your Product Stand Out With WRITING CODE	2019-05-05 14:56:02-07
9	3 Ways To Have (A) More Appealing PROGRAMMING HASKELL	Essential WRITING CODE Smartphone Apps	2018-08-04 00:18:14-07
10	4 Ways You Can Grow Your Creativity Using PROGRAMMING HASKELL	How To Take The Headache Out Of WRITING CODE	2018-05-06 21:21:29-07
11	What Everyone Must Know About PROGRAMMING HASKELL	Don't Just Sit There! Start WRITING CODE	2018-01-05 02:33:44-08
12	The Death Of PROGRAMMING HASKELL And How To Avoid It	Interesting Facts I Bet You Never Knew About WRITING CODE	2017-01-12 09:45:14-08
13	PROGRAMMING HASKELL Your Way To Success	The Best Way To WRITING CODE	2018-06-16 19:24:42-07
14	5 Brilliant Ways To Use PROGRAMMING HASKELL	WRITING CODE Made Simple - Even Your Kids Can Do It	2017-11-27 09:40:21-08
15	Boost Your PROGRAMMING HASKELL With These Tips	WRITING CODE? It's Easy If You Do It Smart	2017-12-05 17:20:24-08
16	12 Questions Answered About PROGRAMMING HASKELL	No More Mistakes With WRITING CODE	2018-04-27 02:19:54-07
17	9 Ways PROGRAMMING HASKELL Can Make You Invincible	Do WRITING CODE Better Than Barack Obama	2017-02-11 11:11:27-08
18	Where Is The Best PROGRAMMING HASKELL?	15 Tips For WRITING CODE Success	2019-04-01 06:40:14-07
19	The Ultimate Guide To PROGRAMMING HASKELL	Take The Stress Out Of WRITING CODE	2017-06-27 14:48:28-07
20	How To Turn PROGRAMMING HASKELL Into Success	Now You Can Have The WRITING CODE Of Your Dreams û Cheaper/Faster Than You Ever Imagined	2018-12-06 00:48:05-08
21	5 Ways PROGRAMMING HASKELL Will Help You Get More Business	Here Is A Method That Is Helping WRITING CODE	2018-12-04 11:00:04-08
22	The Truth About PROGRAMMING HASKELL In 3 Minutes	These 5 Simple WRITING CODE Tricks Will Pump Up Your Sales Almost Instantly	2019-03-28 21:10:08-07
23	Essential PROGRAMMING HASKELL Smartphone Apps	WRITING CODE Works Only Under These Conditions	2018-07-25 14:51:37-07
24	How To Find The Right PROGRAMMING HASKELL For Your Specific Product(Service).	Want A Thriving Business? Focus On WRITING CODE!	2017-01-04 07:09:34-08
25	How To Win Clients And Influence Markets with PROGRAMMING HASKELL	WRITING CODE Is Bound To Make An Impact In Your Business	2019-04-18 00:02:56-07
26	PROGRAMMING HASKELL: This Is What Professionals Do	Why Everything You Know About WRITING CODE Is A Lie	2018-09-16 08:16:30-07
27	Are You Embarrassed By Your PROGRAMMING HASKELL Skills? Here's What To Do	3 Ways Twitter Destroyed My WRITING CODE Without Me Noticing	2019-05-12 19:20:42-07
28	What Zombies Can Teach You About PROGRAMMING HASKELL	5 Simple Steps To An Effective WRITING CODE Strategy	2017-10-29 20:41:13-07
29	Don't Fall For This PROGRAMMING HASKELL Scam	10 Unforgivable Sins Of WRITING CODE	2017-08-09 15:23:12-07
30	Got Stuck? Try These Tips To Streamline Your PROGRAMMING HASKELL	Never Changing WRITING CODE Will Eventually Destroy You	2018-09-16 03:34:08-07
31	PROGRAMMING HASKELL An Incredibly Easy Method That Works For All	Revolutionize Your WRITING CODE With These Easy-peasy Tips	2018-11-15 13:32:16-08
32	Have You Heard? PROGRAMMING HASKELL Is Your Best Bet To Grow	Congratulations! Your WRITING CODE Is (Are) About To Stop Being Relevant	2017-12-15 02:42:10-08
33	10 Warning Signs Of Your PROGRAMMING HASKELL Demise	Where Is The Best WRITING CODE?	2018-01-07 15:38:39-08
34	Answered: Your Most Burning Questions About PROGRAMMING HASKELL	2 Ways You Can Use WRITING CODE To Become Irresistible To Customers	2019-02-19 16:53:45-08
35	5 Simple Steps To An Effective PROGRAMMING HASKELL Strategy	The Philosophy Of WRITING CODE	2018-01-14 03:14:49-08
36	22 Tips To Start Building A PROGRAMMING HASKELL You Always Wanted	How To Save Money with WRITING CODE?	2018-10-31 14:07:56-07
37	Why Most People Will Never Be Great At PROGRAMMING HASKELL	7 Rules About WRITING CODE Meant To Be Broken	2019-01-05 19:13:54-08
38	The Ultimate Deal On PROGRAMMING HASKELL	5 Reasons WRITING CODE Is A Waste Of Time	2019-04-04 03:21:13-07
39	The Secrets To Finding World Class Tools For Your PROGRAMMING HASKELL Quickly	9 Ridiculous Rules About WRITING CODE	2017-12-25 00:38:23-08
40	Don't Be Fooled By PROGRAMMING HASKELL	Fall In Love With WRITING CODE	2018-06-03 19:38:38-07
41	What Can You Do About PROGRAMMING HASKELL Right Now	The Untold Secret To Mastering WRITING CODE In Just 3 Days	2017-05-20 21:51:00-07
42	Double Your Profit With These 5 Tips on PROGRAMMING HASKELL	Is WRITING CODE Worth [$] To You?	2018-06-14 04:22:17-07
43	Ho To (Do) PROGRAMMING HASKELL Without Leaving Your Office(House).	The Secrets To WRITING CODE	2018-08-18 21:27:31-07
44	Top 10 Tips To Grow Your PROGRAMMING HASKELL	3 Easy Ways To Make WRITING CODE Faster	2018-01-09 06:06:02-08
45	5 Sexy Ways To Improve Your PROGRAMMING HASKELL	The WRITING CODE That Wins Customers	2017-01-24 10:03:28-08
46	Clear And Unbiased Facts About PROGRAMMING HASKELL (Without All the Hype)	5 Incredibly Useful WRITING CODE Tips For Small Businesses	2017-06-07 08:07:17-07
47	10 Best Practices For PROGRAMMING HASKELL	Being A Star In Your Industry Is A Matter Of WRITING CODE	2019-03-07 03:24:04-08
48	Find A Quick Way To PROGRAMMING HASKELL	Should Fixing WRITING CODE Take 60 Steps?	2017-05-02 03:20:25-07
49	Being A Star In Your Industry Is A Matter Of PROGRAMMING HASKELL	Who Else Wants To Know The Mystery Behind WRITING CODE?	2018-11-21 21:28:19-08
50	How To Earn $398/Day Using PROGRAMMING HASKELL	WRITING CODE And Love Have 4 Things In Common	2017-01-22 09:40:49-08
51	PROGRAMMING HASKELL Works Only Under These Conditions	In 10 Minutes, I'll Give You The Truth About WRITING CODE	2018-04-14 03:58:12-07
52	17 Tricks About PROGRAMMING HASKELL You Wish You Knew Before	10 Tips That Will Make You Influential In WRITING CODE	2017-11-18 23:27:49-08
53	15 Tips For PROGRAMMING HASKELL Success	The Hidden Mystery Behind WRITING CODE	2018-06-23 23:23:56-07
54	Can You Really Find PROGRAMMING HASKELL (on the Web)?	Fear? Not If You Use WRITING CODE The Right Way!	2018-10-01 17:08:17-07
55	How To Get (A) Fabulous PROGRAMMING HASKELL On A Tight Budget	You Don't Have To Be A Big Corporation To Start WRITING CODE	2017-09-27 00:37:07-07
56	How To Win Friends And Influence People with PROGRAMMING HASKELL	How WRITING CODE Made Me A Better Salesperson	2018-01-20 19:10:34-08
57	Who Else Wants To Know The Mystery Behind PROGRAMMING HASKELL?	Winning Tactics For WRITING CODE	2018-03-05 04:30:19-08
58	How To Start A Business With PROGRAMMING HASKELL	3 Tips About WRITING CODE You Can't Afford To Miss	2017-10-19 14:30:08-07
59	11 Methods Of PROGRAMMING HASKELL Domination	5 Ways Of WRITING CODE That Can Drive You Bankrupt - Fast!	2018-05-01 22:24:52-07
60	3 Ways You Can Reinvent PROGRAMMING HASKELL Without Looking Like An Amateur	ThereÆs Big Money In WRITING CODE	2017-10-16 00:07:14-07
61	10 Tips That Will Make You Influential In PROGRAMMING HASKELL	The Number One Reason You Should (Do) WRITING CODE	2018-09-12 10:41:42-07
62	Don't Just Sit There! Start PROGRAMMING HASKELL	It's All About (The) WRITING CODE	2017-06-25 12:11:13-07
63	Everything You Wanted to Know About PROGRAMMING HASKELL and Were Afraid To Ask	3 Ways To Have (A) More Appealing WRITING CODE	2019-03-08 11:06:00-08
64	Fall In Love With PROGRAMMING HASKELL	WRITING CODE: An Incredibly Easy Method That Works For All	2018-07-09 23:29:59-07
65	Death, PROGRAMMING HASKELL And Taxes	Here Is A Quick Cure For WRITING CODE	2018-12-21 00:27:14-08
66	Why I Hate PROGRAMMING HASKELL	3 WRITING CODE Secrets You Never Knew	2018-07-19 23:48:14-07
67	Listen To Your Customers. They Will Tell You All About PROGRAMMING HASKELL	What Can Instagramm Teach You About WRITING CODE	2017-01-03 23:02:40-08
68	Fascinating PROGRAMMING HASKELL Tactics That Can Help Your Business Grow	To People That Want To Start WRITING CODE But Are Affraid To Get Started	2019-02-04 09:07:48-08
69	10 Unforgivable Sins Of PROGRAMMING HASKELL	Where Can You Find Free WRITING CODE Resources	2018-05-15 03:22:07-07
70	It's All About (The) PROGRAMMING HASKELL	Can You Really Find WRITING CODE (on the Web)?	2017-08-26 02:13:46-07
71	Avoid The Top 10 PROGRAMMING HASKELL Mistakes	5 Secrets: How To Use WRITING CODE To Create A Successful Business(Product)	2017-12-13 03:02:28-08
72	5 Best Ways To Sell PROGRAMMING HASKELL	WRITING CODE An Incredibly Easy Method That Works For All	2018-03-17 20:47:42-07
73	Little Known Ways to PROGRAMMING HASKELL	5 Sexy Ways To Improve Your WRITING CODE	2019-03-12 07:18:08-07
74	2 Ways You Can Use PROGRAMMING HASKELL To Become Irresistible To Customers	Believe In Your WRITING CODE Skills But Never Stop Improving	2017-04-13 01:11:06-07
75	The Secret of Successful PROGRAMMING HASKELL	Get The Most Out of WRITING CODE and Facebook	2017-08-01 08:24:54-07
76	I Don't Want To Spend This Much Time On PROGRAMMING HASKELL. How About You?	14 Days To A Better WRITING CODE	2017-10-05 13:16:26-07
77	Stop Wasting Time And Start PROGRAMMING HASKELL	Secrets To Getting WRITING CODE To Complete Tasks Quickly And Efficiently	2017-03-17 01:58:47-07
78	5 Ways Of PROGRAMMING HASKELL That Can Drive You Bankrupt - Fast!	The Quickest & Easiest Way To WRITING CODE	2019-05-17 14:36:38-07
79	Marriage And PROGRAMMING HASKELL Have More In Common Than You Think	The Death Of WRITING CODE And How To Avoid It	2019-01-25 22:16:55-08
80	PROGRAMMING HASKELL And Love - How They Are The Same	Take 10 Minutes to Get Started With WRITING CODE	2018-07-15 20:41:40-07
81	OMG! The Best PROGRAMMING HASKELL Ever!	5 Easy Ways You Can Turn WRITING CODE Into Success	2017-12-03 04:51:25-08
82	Find Out How I Cured My PROGRAMMING HASKELL In 2 Days	WRITING CODE: The Samurai Way	2019-03-14 08:49:30-07
83	Remarkable Website - PROGRAMMING HASKELL Will Help You Get There	What Is WRITING CODE and How Does It Work?	2018-10-24 01:27:25-07
84	Is PROGRAMMING HASKELL Worth [$] To You?	How To Lose Money With WRITING CODE	2018-09-21 01:28:46-07
85	You Can Thank Us Later - 3 Reasons To Stop Thinking About PROGRAMMING HASKELL	How To Learn WRITING CODE	2017-07-31 09:42:40-07
86	How To Use PROGRAMMING HASKELL To Desire	How To Start A Business With WRITING CODE	2018-01-07 19:46:41-08
87	Learn To (Do) PROGRAMMING HASKELL Like A Professional	5 Things To Do Immediately About WRITING CODE	2017-05-26 12:02:31-07
88	PROGRAMMING HASKELL Smackdown!	3 Ways To Master WRITING CODE Without Breaking A Sweat	2017-03-16 22:49:21-07
89	Wondering How To Make Your PROGRAMMING HASKELL Rock? Read This!	3 Things Everyone Knows About WRITING CODE That You Don't	2018-12-27 16:41:19-08
90	5 Ways You Can Get More PROGRAMMING HASKELL While Spending Less	This Study Will Perfect Your WRITING CODE: Read Or Miss Out	2019-03-30 05:45:11-07
91	The Ugly Truth About PROGRAMMING HASKELL	The Truth Is You Are Not The Only Person Concerned About WRITING CODE	2019-02-10 10:10:32-08
92	The Single Most Important Thing You Need To Know About PROGRAMMING HASKELL	Proof That WRITING CODE Really Works	2017-05-28 14:49:54-07
93	A Guide To PROGRAMMING HASKELL At Any Age	Learn Exactly How We Made WRITING CODE Last Month	2017-11-30 21:51:15-08
94	Sexy PROGRAMMING HASKELL	WRITING CODE Iphone Apps	2018-01-13 16:43:30-08
95	Top 3 Ways To Buy A Used PROGRAMMING HASKELL	WRITING CODE Smackdown!	2019-01-24 16:11:59-08
96	World Class Tools Make PROGRAMMING HASKELL Push Button Easy	How To Teach WRITING CODE Better Than Anyone Else	2017-06-27 09:04:46-07
97	Top 10 Tips With PROGRAMMING HASKELL	What Alberto Savoia Can Teach You About WRITING CODE	2017-11-03 08:10:07-07
98	The Ultimate Secret Of PROGRAMMING HASKELL	Marriage And WRITING CODE Have More In Common Than You Think	2017-03-05 14:21:16-08
99	Now You Can Have Your PROGRAMMING HASKELL Done Safely	How To Turn WRITING CODE Into Success	2018-04-25 17:58:23-07
100	Apply These 5 Secret Techniques To Improve PROGRAMMING HASKELL	5 Problems Everyone Has With WRITING CODE û How To Solved Them	2018-10-11 08:41:38-07
101	The Secret of PROGRAMMING HASKELL	Apply These 5 Secret Techniques To Improve WRITING CODE	2017-02-08 21:22:58-08
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
50	Santa Washinton	Santa@Washinton.com	55
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
80	Romaine Trent	Romaine@Trent.com	46
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
100	Marlena Watt	Marlena@Watt.com	53
101	Glayds Blackford	Glayds.Blackford@test.com	51
\.


--
-- Name: articles_id_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('public.articles_id_seq', 101, true);


--
-- Name: users_id_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('public.users_id_seq', 101, true);


--
-- Name: articles articles_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.articles
    ADD CONSTRAINT articles_pkey PRIMARY KEY (id);


--
-- Name: users users_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.users
    ADD CONSTRAINT users_pkey PRIMARY KEY (id);


--
-- PostgreSQL database dump complete
--

