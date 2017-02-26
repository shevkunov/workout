import vk
session = vk.Session()
api = vk.API(session)

print(len(api.friends.get(user_id=1)))
