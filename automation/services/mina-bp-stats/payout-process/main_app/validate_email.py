import psycopg2
from datetime import timezone
import datetime
from payouts_config import BaseConfig
import pandas as pd
from sendgrid import SendGridAPIClient
from sendgrid.helpers.mail import Mail, Attachment, FileContent, FileName, FileType, Disposition
from logger_util import logger

connection_leaderboard = psycopg2.connect(
    host=BaseConfig.POSTGRES_LEADERBOARD_HOST,
    port=BaseConfig.POSTGRES_LEADERBOARD_PORT,
    database=BaseConfig.POSTGRES_LEADERBOARD_DB,
    user=BaseConfig.POSTGRES_LEADERBOARD_USER,
    password=BaseConfig.POSTGRES_LEADERBOARD_PASSWORD
)

logger.info('validate email send to accounts ')


def get_block_producer_mail(winner_bpk, conn=connection_leaderboard):
    mail_id_sql = """select block_producer_email from node_record_table where block_producer_key = %s"""
    cursor = conn.cursor()
    try:
        cursor.execute(mail_id_sql, (winner_bpk,))
    except (Exception, psycopg2.DatabaseError) as error:
        logger.info("Error: {0} ", format(error))
        cursor.close()
        return 1
    data = cursor.fetchall()
    #email = data[-1][-1]
    email = "umesh.bihani@bnt-soft.com"
    return email


def second_mail(email_df):
    # read the data from delegation_record_table
    payouts_df = email_df
    deadline_date = datetime.datetime.now(timezone.utc) + datetime.timedelta(days=1)
    deadline_date = deadline_date.strftime("%d-%m-%Y %H:%M:%S")
    count = 1
    for i in range(payouts_df.shape[0]):
        count = count + 1
        # 0- provider_pub_key, 1- winner_pub_key, 2- payout_amount
        html_content2 = """<!DOCTYPE html>
        <html lang="en">
          <head>
            <meta charset="UTF-8" />
            <meta http-equiv="X-UA-Compatible" content="IE=edge" />
            <meta name="viewport" content="width=device-width, initial-scale=1.0" />
            <title>Email Template 2</title>
            <link
              rel="stylesheet"
              href="https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0/css/bootstrap.min.css"
              integrity="sha384-Gn5384xqQ1aoWXA+058RXPxPg6fy4IWvTNh0E263XmFcJlSAwiGgFAW/dAiS6JXm"
              crossorigin="anonymous"
            />
            <script
              src="https://code.jquery.com/jquery-3.2.1.slim.min.js"
              integrity="sha384-KJ3o2DKtIkvYIK3UENzmM7KCkRr/rE9/Qpg6aAZGJwFDMVNA/GpGFF93hXpG5KkN"
              crossorigin="anonymous"
            ></script>
            <script
              src="https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.12.9/umd/popper.min.js"
              integrity="sha384-ApNbgh9B+Y1QKtv3Rn7W3mgPxhU9K/ScQsAP7hUibX39j7fakFPskvXusvfa0b4Q"
              crossorigin="anonymous"
            ></script>
            <script
              src="https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0/js/bootstrap.min.js"
              integrity="sha384-JZR6Spejh4U02d8jOt6vLEHfe/JQGiRRSQQxSfFWpi1MquVdAyjUar5+76PVCmYl"
              crossorigin="anonymous"
            ></script>
            <link
              rel="stylesheet"
              href="https://cdn.jsdelivr.net/npm/bootstrap-icons@1.4.0/font/bootstrap-icons.css"
            />
            <link rel="preconnect" href="https://fonts.gstatic.com" />
            <link
              href="https://fonts.googleapis.com/css2?family=IBM+Plex+Sans:ital,wght@0,100;0,200;0,300;0,400;0,500;0,600;0,700;1,100;1,200;1,300;1,400;1,500;1,600;1,700&display=swap"
              rel="stylesheet"
            />
            <!-- <link rel="stylesheet" href="assets/css/emailTemplate.css" /> -->
            <style>
              a {
          text-decoration: underline;
        }
        ul {
          list-style-type: none;
        }
        ul li:before {
          content: "\2212";
          position: absolute;
          margin-left: -20px;
        }

        ul li ol li:before {
          content: "";
          position: absolute;
          margin-left: -20px;
        }

        ul li ol li ul li:before {
          content: "\2212";
          position: absolute;
          margin-left: -20px;
        }

        .bg-warning {
          background-color: unset!important;
        }

        .container {
          font-family: "IBM Plex Sans", sans-serif;
          font-style: normal;
          font-weight: light;
          /* font-size: 10px; */
          margin: 4rem;
          /* margin-top: 5rem; */
        }

        .inner-container {
          padding: 0px;
          margin: 10px;
        }
        .dynamic-text-color {
          color: #ec3a61;
        }
        .rules {
          padding-right: 6rem;
        }

        .warning-icon {
          background-color: #f5f846;
        }

        p i .warning {
          max-width: 256px;
          max-height: 256px;
          background-image: url(data:image/svg+xml;base64,PD94bWwgdmVyc2lvbj0iMS4wIiBlbmNvZGluZz0iaXNvLTg4NTktMSI/Pg0KPCEtLSBHZW5lcmF0b3I6IEFkb2JlIElsbHVzdHJhdG9yIDE5LjAuMCwgU1ZHIEV4cG9ydCBQbHVnLUluIC4gU1ZHIFZlcnNpb246IDYuMDAgQnVpbGQgMCkgIC0tPg0KPHN2ZyB2ZXJzaW9uPSIxLjEiIGlkPSJDYXBhXzEiIHhtbG5zPSJodHRwOi8vd3d3LnczLm9yZy8yMDAwL3N2ZyIgeG1sbnM6eGxpbms9Imh0dHA6Ly93d3cudzMub3JnLzE5OTkveGxpbmsiIHg9IjBweCIgeT0iMHB4Ig0KCSB2aWV3Qm94PSIwIDAgNTEyIDUxMiIgc3R5bGU9ImVuYWJsZS1iYWNrZ3JvdW5kOm5ldyAwIDAgNTEyIDUxMjsiIHhtbDpzcGFjZT0icHJlc2VydmUiPg0KPHBhdGggc3R5bGU9ImZpbGw6IzNCNDE0NTsiIGQ9Ik0zMjIuOTM5LDYyLjY0MmwxNzguNzM3LDMwOS41ODNDNTA4LjIzMSwzODMuNTc4LDUxMiwzOTYuNzQsNTEyLDQxMC43OTENCgljMCw0Mi42Ny0zNC41OTIsNzcuMjY0LTc3LjI2NCw3Ny4yNjRIMjU2TDE5NC4xODksMjU2TDI1NiwyMy45NDZDMjg0LjYyLDIzLjk0NiwzMDkuNTg3LDM5LjUxOSwzMjIuOTM5LDYyLjY0MnoiLz4NCjxwYXRoIHN0eWxlPSJmaWxsOiM1MjVBNjE7IiBkPSJNMTg5LjA2MSw2Mi42NDJMMTAuMzIzLDM3Mi4yMjVDMy43NjksMzgzLjU3OCwwLDM5Ni43NCwwLDQxMC43OTENCgljMCw0Mi42NywzNC41OTIsNzcuMjY0LDc3LjI2NCw3Ny4yNjRIMjU2VjIzLjk0NkMyMjcuMzgsMjMuOTQ2LDIwMi40MTMsMzkuNTE5LDE4OS4wNjEsNjIuNjQyeiIvPg0KPHBhdGggc3R5bGU9ImZpbGw6I0ZGQjc1MTsiIGQ9Ik00NzQuOTEzLDM4Ny42NzhMMjk2LjE3Nyw3OC4wOThjLTguMDU2LTEzLjk1OS0yMi44NDktMjIuNzY3LTM4Ljg0OC0yMy4yMmwxNTIuODY5LDQwMi4yNzVoMjQuNTM5DQoJYzI1LjU1OSwwLDQ2LjM1OC0yMC43OTgsNDYuMzU4LTQ2LjM1OEM0ODEuMDk1LDQwMi42NzcsNDc4Ljk1MiwzOTQuNjgzLDQ3NC45MTMsMzg3LjY3OHoiLz4NCjxwYXRoIHN0eWxlPSJmaWxsOiNGRkQ3NjQ7IiBkPSJNNDQ0Ljg1MywzODcuNjc4YzMuNDkyLDcuMDA1LDUuMzM2LDE0Ljk5OSw1LjMzNiwyMy4xMTdjMCwyNS41NTktMTcuOTM1LDQ2LjM1OC0zOS45OTIsNDYuMzU4DQoJSDc3LjI2NGMtMjUuNTU5LDAtNDYuMzU4LTIwLjc5OS00Ni4zNTgtNDYuMzU4YzAtOC4xMTgsMi4xNDMtMTYuMTEyLDYuMTgxLTIzLjExN2wxNzguNzM2LTMwOS41OA0KCWM4LjI4My0xNC4zNCwyMy42NzQtMjMuMjUxLDQwLjE3Ny0yMy4yNTFjMC40NDMsMCwwLjg4NiwwLjAxLDEuMzI5LDAuMDMxYzEzLjczMiwwLjUzNiwyNi40MTQsOS4zMjMsMzMuMzI2LDIzLjIyTDQ0NC44NTMsMzg3LjY3OHoNCgkiLz4NCjxwYXRoIHN0eWxlPSJmaWxsOiMzQjQxNDU7IiBkPSJNMjU2LDM1NC4xMzF2NTEuNTA5YzE0LjIyNywwLDI1Ljc1NS0xMS41MjgsMjUuNzU1LTI1Ljc1NQ0KCUMyODEuNzU1LDM2NS42NTksMjcwLjIyNywzNTQuMTMxLDI1NiwzNTQuMTMxeiIvPg0KPHBhdGggc3R5bGU9ImZpbGw6IzUyNUE2MTsiIGQ9Ik0yNTYsMzU0LjEzMWMyLjg0MywwLDUuMTUxLDExLjUyOCw1LjE1MSwyNS43NTVjMCwxNC4yMjctMi4zMDgsMjUuNzU1LTUuMTUxLDI1Ljc1NQ0KCWMtMTQuMjI3LDAtMjUuNzU1LTExLjUyOC0yNS43NTUtMjUuNzU1QzIzMC4yNDUsMzY1LjY1OSwyNDEuNzczLDM1NC4xMzEsMjU2LDM1NC4xMzF6Ii8+DQo8cGF0aCBzdHlsZT0iZmlsbDojM0I0MTQ1OyIgZD0iTTI1NiwxMzIuNjQ2VjMyMy4yM2MxNC4yMjcsMCwyNS43NTUtMTEuNTM4LDI1Ljc1NS0yNS43NTVWMTU4LjQwMQ0KCUMyODEuNzU1LDE0NC4xNzQsMjcwLjIyNywxMzIuNjQ2LDI1NiwxMzIuNjQ2eiIvPg0KPHBhdGggc3R5bGU9ImZpbGw6IzUyNUE2MTsiIGQ9Ik0yNTYsMTMyLjY0NmMyLjg0MywwLDUuMTUxLDExLjUyOCw1LjE1MSwyNS43NTV2MTM5LjA3NGMwLDE0LjIxNi0yLjMwOCwyNS43NTUtNS4xNTEsMjUuNzU1DQoJYy0xNC4yMjcsMC0yNS43NTUtMTEuNTM4LTI1Ljc1NS0yNS43NTVWMTU4LjQwMUMyMzAuMjQ1LDE0NC4xNzQsMjQxLjc3MywxMzIuNjQ2LDI1NiwxMzIuNjQ2eiIvPg0KPGc+DQo8L2c+DQo8Zz4NCjwvZz4NCjxnPg0KPC9nPg0KPGc+DQo8L2c+DQo8Zz4NCjwvZz4NCjxnPg0KPC9nPg0KPGc+DQo8L2c+DQo8Zz4NCjwvZz4NCjxnPg0KPC9nPg0KPGc+DQo8L2c+DQo8Zz4NCjwvZz4NCjxnPg0KPC9nPg0KPGc+DQo8L2c+DQo8Zz4NCjwvZz4NCjxnPg0KPC9nPg0KPC9zdmc+DQo=);
        }

        .section-page-2 {
          padding-right: 5rem!important;
        }

        .delegation-policy_format{
          font-family: Arial;
          color: #7f7f7f;
        }
            </style>
          </head>
          <body>
            <div class="container">
                <div class="inner-container">

              <section class="px-5 section-page-2">
               <p>Dear Mina community member,</p>

               <p>There are <span class="font-weight-bold"><b>24 hours left</b></span> to send the staking rewards of the delegation program back to the Mina Foundation according to the <a href="https://minaprotocol.com/blog/mina-foundation-delegation-policy" target="_blank">delegation policy</a>.</p>
              <p class="font-italic">Feel free to ignore this email if you have already sent back the staking rewards. We recommend you use a block explorer to check if your transactions were completed.</p>
              <p>You are receiving a delegation from two addresses of the Mina Foundation. This email is related to the delegation from address <span class="bg-warning">""" + f"""{payouts_df.iloc[i, 0]}""" + """</span>.</p>
              <p><svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" fill="#f1950b" class="bi bi-exclamation-triangle-fill" viewBox="0 0 16 16">
                <path d="M8.982 1.566a1.13 1.13 0 0 0-1.96 0L.165 13.233c-.457.778.091 1.767.98 1.767h13.713c.889 0 1.438-.99.98-1.767L8.982 1.566zM8 5c.535 0 .954.462.9.995l-.35 3.507a.552.552 0 0 1-1.1 0L7.1 5.995A.905.905 0 0 1 8 5zm.002 6a1 1 0 1 1 0 2 1 1 0 0 1 0-2z"/>
              </svg></i> If you haven’t done already, please send the staking rewards <span class="font-weight-bold"><b>immediately</b></span>, as soon as you read this email. You are allowed to keep up to 5% staking rewards. <span class="font-weight-bold"><b>The deadline</b></span> to send back rewards is in <span class="font-weight-bold"><b>24 hours</b></span>, at slot number 3500.</p>

              <p class="rules">
                  <ul class="dashed rules">
                    <li>According to our system, you are required to send back at least 
                        <ul class="dashed">
                            <li><span class="bg-warning">""" + f"""{payouts_df.iloc[i, 2]}""" + """</span> minas to this address: <span class="bg-warning">""" + f"""{payouts_df.iloc[i, 0]}""" + """</span>
                            </li>
                            <li class="dynamic-text-color">In order for us to associate the transaction with your delegation, please do one of the following in the transaction where you pay back rewards: 
                                <ol type="a" start="5">
                                    <li>send the transaction from the <span class="font-italic">hot wallet</span>  <span class="bg-warning text-dark">""" + f"""{payouts_df.iloc[i, 0]}""" + """</span> that the Mina Foundation is delegating to.</li>
                                     <li>send the transaction from the coinbase-receiver account that was specified in any of the blocks that your account has created.</li>
                                    <li>put the <span class="font-italic">discord id</span> that you signed up for the program with in the <span class="font-italic">memo</span>.</li>
                                    <li>put the <span class="font-italic">sha256</span> of your discord id in the <span class="font-italic">memo</span>.</li>
                                    <li>put the <span class="font-italic">sha256</span> of your <span class="font-italic">hot wallet</span> address in the <span class="font-italic">memo</span>.</li>
                                </ol>
                            </li>
                            <li>Details related to the calculation of the payout can be found <a href="https://docs.minaprotocol.com/en/advanced/foundation-delegation-program" target="_blank">here</a>.</li>
                        </ul>
                    </li>

                    <li><span class="font-weight-bold"></b>Failure to do so will result in penalization and undelegation.</b></span> Please refer to the <a href="https://minaprotocol.com/blog/mina-foundation-delegation-policy" target="_blank">delegation policy</a>  for more details.</li>
                    <li>We recommend you use a block explorer to check if your transactions were completed.</li>
                  </ul>
              </p>
              <p>Please complete the (remaining) payment <span class="font-weight-bold"><b>IMMEDIATELY, as soon as you read this email</b></span>. If you do not comply with the requirements <span class="font-weight-bold"><b>within 24 HOURS</b></span> before the deadline (slot 3500) on <span class="font-weight-bold bg-warning"><b>""" + f"""{deadline_date}""" + """,</b></span> you will be penalized, and face undelegation.<span class="font-weight-bold"><b>We take violations seriously</b></span> .</p>


               <p>Please be reminded of the following clause in the delegation policy: </p>
               <div class="delegation-policy_format font-italic"><p>"c) Once every quarter, or more frequently as determined by the Mina Foundation, the Mina Foundation will review the list of validators to determine whether un-/re-delegation is appropriate with respect to any particular validator, taking into account the following exclusive factors:<br> ...<br></p>
                <p>- whether any validator did not properly remit back the staking rewards; and"</p></div>
                <p>If you have any questions related to the delegation program, you can ask them in the <br> <span class="font-weight-bold"> <b>#delegation-program channel</b> </span> on <a href="http://bit.ly/MinaDiscord" target="_blank">Discord</a>. <br>
                </p>
                <p>Best regards,</p>
                <p>Mina Foundation</p>



            </section>
            </div>
            </div>
          </body>
        </html>
        """

        subject = "🚨[ACTION REQUIRED ] 24 HOURS LEFT to Send Block Rewards in MINAS for the Delegation"
        block_producer_email = get_block_producer_mail(payouts_df.iloc[i, 1])

        message = Mail(from_email=BaseConfig.FROM_EMAIL,
                       to_emails=block_producer_email,
                       subject=subject,
                       plain_text_content='text',
                       html_content=html_content2)

        try:
            sg = SendGridAPIClient(api_key=BaseConfig.SENDGRID_API_KEY)
            response = sg.send(message)
            logger.info(response.status_code)
            logger.info(response.body)
            logger.info(response.headers)
        except Exception as e:
            logger.info(e)
    print("emails sent: ", count)

